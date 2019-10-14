module Tiger.Semant.Types where

import           Control.Monad.Error.Class
import           Control.Monad.State.Class
import           Data.Extensible
import           Data.Extensible.Effect.Default
import           RIO
import qualified RIO.Partial as Partial
import qualified RIO.Map as Map
import qualified RIO.List.Partial as List

import qualified Env as E
import qualified Frame as F
import           Id
import           IR
import           Unique

data Type = TUnit
          | TInt
          | TString
          | TNil
          | TRecord (Record '["map" :> Map.Map Id Type, "id" :> Unique])
          | TArray (Record '["range" :> Type, "id" :> Unique])
          | TName LId
          deriving (Show)
instance Eq Type where
  TUnit == TUnit = True
  TInt == TInt = True
  TString == TString = True
  TNil == TNil = True
  (TRecord r) == (TRecord r') = r ^. #id == r' ^. #id
  (TArray a) == (TArray a') = a ^. #id == a' ^. #id
  _ == _ = False
instance Ord Type where
  (TRecord _) <= TNil = True
  ty <= ty' = ty == ty'
isComparable :: Type -> Type -> Bool
isComparable leftTy rightTy = leftTy <= rightTy || rightTy <= leftTy


data Level f = TopLevel | Level (Record '["unique" >: Unique, "frame" >: f]) deriving (Show)
newtype NestingLevel f = NestingLevel [Level f]
outermost :: NestingLevel f
outermost = NestingLevel [TopLevel]
pushNestingLevel :: Level f -> NestingLevel f -> NestingLevel f
pushNestingLevel level (NestingLevel levels) = NestingLevel $ level : levels
popNestingLevel :: NestingLevel f -> Maybe (Level f, NestingLevel f)
popNestingLevel (NestingLevel []) = Nothing
popNestingLevel (NestingLevel (level:levels)) = Just (level, NestingLevel levels)
pullInStaticLinks :: forall f. F.Frame f => Level f -> NestingLevel f -> Maybe IR.Exp
pullInStaticLinks level current = leaveEff . runMaybeDef . (`runReaderDef` level) . (`evalStateDef` IR.Temp (F.fp @f)) $ pullInStaticLinksInternal current
  where
    pullInStaticLinksInternal level = case popNestingLevel level of
      Just (Level current, levels) -> ask >>= \case
        TopLevel -> throwError ()
        Level target -> if target ^. #unique == current ^. #unique
          then get
          else do
            modify $ F.exp (List.head $ F.formals (current ^. #frame))
            pullInStaticLinksInternal levels
      _ -> throwError ()
takeParametersAccess :: F.Frame f => Level f -> Maybe [F.Access f]
takeParametersAccess TopLevel = Nothing
takeParametersAccess (Level r) = Just . List.tail . F.formals $ r ^. #frame

type NestingLevelEff f = State (Record '["unique" >: Unique, "level" >: NestingLevel f])
runNestingLevelEff :: Eff (("nestingLevel" >: NestingLevelEff f) ': xs) a -> Eff xs a
runNestingLevelEff = flip evalStateEff (#unique @= uniqueSeed <: #level @= outermost <: nil)
getNestingLevelEff :: Lookup xs "nestingLevel" (NestingLevelEff f) => Eff xs (NestingLevel f)
getNestingLevelEff = getsEff #nestingLevel (^. #level)
pushLevelEff :: Lookup xs "nestingLevel" (NestingLevelEff f) => Level f -> Eff xs ()
pushLevelEff level = modifyEff #nestingLevel $ over #level (pushNestingLevel level)
popLevelEff :: Lookup xs "nestingLevel" (NestingLevelEff f) => Eff xs (Maybe (Level f))
popLevelEff = do
  levels <- getNestingLevelEff
  case popNestingLevel levels of
    Nothing -> pure Nothing
    Just (level, levels) -> do
       modifyEff #nestingLevel $ set #level levels
       pure $ Just level
fetchCurrentLevelEff :: Lookup xs "nestingLevel" (NestingLevelEff f) => Eff xs (Level f)
fetchCurrentLevelEff = fst . Partial.fromJust . popNestingLevel <$> getNestingLevelEff
modifyCurrentLevelEff :: Lookup xs "nestingLevel" (NestingLevelEff f) => (Level f -> Level f) -> Eff xs ()
modifyCurrentLevelEff f = popLevelEff >>= \case
  Nothing -> pure ()
  Just level -> pushLevelEff $ f level
pullInStaticLinksEff :: (Lookup xs "nestingLevel" (NestingLevelEff f), F.Frame f) => Level f -> Eff xs IR.Exp
pullInStaticLinksEff level = Partial.fromJust . pullInStaticLinks level <$> getNestingLevelEff
fetchCurrentLevelParametersAccessEff :: (F.Frame f, Lookup xs "nestingLevel" (NestingLevelEff f)) => Eff xs [F.Access f]
fetchCurrentLevelParametersAccessEff = Partial.fromJust . takeParametersAccess <$> fetchCurrentLevelEff



withNewLevelEff :: forall f xs a. (Lookup xs "temp" UniqueEff, Lookup xs "nestingLevel" (NestingLevelEff f), F.Frame f) => Label -> [Bool] -> Eff xs a -> Eff xs a
withNewLevelEff label formals a = do
  u <- getUniqueEff #nestingLevel
  frame <- F.newFrame label (True : formals)
  let level = Level $ #unique @= u <: #frame @= frame <: nil
  pushLevelEff level
  ret <- a
  _ <- popLevelEff
  pure ret
withLevelEff :: (Lookup xs "nestingLevel" (NestingLevelEff f)) => Level f -> Eff xs a -> Eff xs a
withLevelEff level a = do
  pushLevelEff level
  ret <- a
  _ <- popLevelEff
  pure ret
allocateLocalOnCurrentLevel :: (Lookup xs "temp" UniqueEff, F.Frame f, Lookup xs "nestingLevel" (NestingLevelEff f)) => Bool -> Eff xs (F.Access f)
allocateLocalOnCurrentLevel b = fetchCurrentLevelEff >>= \case
  Level level -> do
    (frame', access) <- F.allocLocal (level ^. #frame) b
    modifyCurrentLevelEff (const . Level $ set #frame  frame' level)
    pure access


newtype Access f = Access (Record '["level" >: Level f, "access" >: F.Access f])
data Var f = Var (Record '["type" >: Type, "access" >: Access f])
         | Fun (Record '["label" :> Label, "level" :> Level f, "domains" :> [Type], "codomain" :> Type])

type TEnv = E.Env Type
type VEnv f = E.Env (Var f)
type HasEnv xs f = (Lookup xs "varEnv" (State (VEnv f)), Lookup xs "typeEnv" (State TEnv))
evalTEnvEff :: TEnv -> Eff (("typeEnv" >: State TEnv) ': xs) a -> Eff xs a
evalTEnvEff = flip (evalStateEff @"typeEnv")
evalVEnvEff :: VEnv f -> Eff (("varEnv" >: State (VEnv f)) ': xs) a -> Eff xs a
evalVEnvEff = flip (evalStateEff @"varEnv")
evalEnvEff :: TEnv -> VEnv f -> Eff (("typeEnv" >: State TEnv) ': ("varEnv" >: State (VEnv f)) ': xs) a -> Eff xs a
evalEnvEff typeEnv varEnv = evalVEnvEff varEnv . evalTEnvEff typeEnv

lookupTypeId :: Lookup xs "typeEnv" (State TEnv) => Id -> Eff xs (Maybe Type)
lookupTypeId id = getsEff #typeEnv $ E.lookup id
lookupVarId :: Lookup xs "varEnv" (State (VEnv f)) => Id -> Eff xs (Maybe (Var f))
lookupVarId id = getsEff #varEnv $ E.lookup id
insertType :: (Lookup xs "typeEnv" (State TEnv)) => Id -> Type -> Eff xs ()
insertType id ty = modifyEff #typeEnv $ E.insert id ty
insertVar :: (Lookup xs "varEnv" (State (VEnv f))) => Id -> Var f -> Eff xs ()
insertVar id v = modifyEff #varEnv $ E.insert id v
withTEnvScope :: (Lookup xs "typeEnv" (State TEnv)) => Eff xs a -> Eff xs a
withTEnvScope = E.withEnvScope #typeEnv
withVEnvScope :: (Lookup xs "varEnv" (State (VEnv f))) => Eff xs a -> Eff xs a
withVEnvScope = E.withEnvScope #varEnv
