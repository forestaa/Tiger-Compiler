module Tiger.Semant.Translate where

import RIO
import qualified RIO.List.Partial as List
import Control.Monad.State.Strict
import Control.Monad.Error.Class
import qualified IR
import qualified Unique as U

import qualified Env as E
import qualified Frame as F
import qualified Tiger.LSyntax as T

import Id
import SrcLoc
import Data.Extensible
import Data.Extensible.Effect.Default


newtype Level f = Level (Record '["unique" >: U.Unique, "frame" >: f])
newtype NestingLevel f = NestingLevel [Level f]
outermost :: NestingLevel f
outermost = NestingLevel []
pushNestingLevel :: Level f -> NestingLevel f -> NestingLevel f
pushNestingLevel level (NestingLevel levels) = NestingLevel $ level : levels
popNestingLevel :: NestingLevel f -> Maybe (Level f, NestingLevel f)
popNestingLevel (NestingLevel []) = Nothing
popNestingLevel (NestingLevel (level:levels)) = Just (level, NestingLevel levels)
pullInStaticLinks :: forall f. F.Frame f => Level f -> NestingLevel f -> Maybe IR.Exp
pullInStaticLinks level current = leaveEff . runMaybeDef . (`runReaderDef` level) . (`evalStateDef` IR.Temp (F.fp (Proxy :: Proxy f))) $ pullInStaticLinksInternal current
  where
    pullInStaticLinksInternal level = case popNestingLevel level of
      Nothing -> throwError ()
      Just (Level current, levels) -> do
        Level target <- ask
        if target ^. #unique == current ^. #unique
          then get
          else do
            modify $ F.exp (Proxy :: Proxy f) (List.head $ F.formals (current ^. #frame))
            pullInStaticLinksInternal levels


type NestingLevelEff f = State (Record '["unique" >: U.Unique, "level" >: NestingLevel f])
runNestingLevelEff :: Eff (("nestingLevel" >: NestingLevelEff f) ': xs) a -> Eff xs a
runNestingLevelEff = flip evalStateEff (#unique @= U.uniqueSeed <: #level @= outermost <: nil)
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
fetchCurrentLevelEff :: Lookup xs "nestingLevel" (NestingLevelEff f) => Eff xs (Maybe (Level f))
fetchCurrentLevelEff = fmap fst . popNestingLevel <$> getNestingLevelEff
modifyCurrentLevelEff :: Lookup xs "nestingLevel" (NestingLevelEff f) => (Level f -> Level f) -> Eff xs ()
modifyCurrentLevelEff f = popLevelEff >>= \case
  Nothing -> pure ()
  Just level -> pushLevelEff $ f level
pullInStaticLinksEff :: (Lookup xs "nestingLevel" (NestingLevelEff f), F.Frame f) => Level f -> Eff xs (Maybe IR.Exp)
pullInStaticLinksEff level = pullInStaticLinks level <$> getNestingLevelEff


newLevel :: (Lookup xs "temp" U.UniqueEff, Lookup xs "nestingLevel" (NestingLevelEff f), F.Frame f) => U.Label -> [Bool] -> Eff xs (Level f)
newLevel label formals = do
  u <- U.getUniqueEff #nestingLevel
  frame <- F.newFrame label (True : formals)
  let level = Level $ #unique @= u <: #frame @= frame <: nil
  pushLevelEff level
  pure level
allocateLocalOnCurrentLevel :: (Lookup xs "temp" U.UniqueEff, F.Frame f, Lookup xs "nestingLevel" (NestingLevelEff f)) => Bool -> Eff xs (Maybe (F.Access f))
allocateLocalOnCurrentLevel b = fetchCurrentLevelEff >>= \case
    Nothing -> pure Nothing
    Just (Level level) -> do
      (frame', access) <- F.allocLocal (level ^. #frame) b
      modifyCurrentLevelEff (const . Level $ set #frame  frame' level)
      pure $ Just access

data Access f = Access (Level f) (F.Access f)

data Exp = Ex IR.Exp | Nx IR.Stm | Cx (U.Label -> U.Label -> IR.Stm)
instance Eq Exp where
  Ex e == Ex e' = e == e'
  Nx s == Nx s' = s == s'
  Cx c == Cx c' = c (U.Label "true" (U.Unique 0)) (U.Label "false" (U.Unique 1)) == c' (U.Label "true" (U.Unique 0)) (U.Label "false" (U.Unique 1))
  _ == _ = False
instance Show Exp where
  show (Ex e) = "Ex: " ++ show e
  show (Nx s) = "Nx: " ++ show s
  show (Cx _) = "Cx"

unEx :: (Lookup xs "label" U.UniqueEff, Lookup xs "temp" U.UniqueEff) => Exp -> Eff xs IR.Exp
unEx (Ex e) = pure e
unEx (Nx s) = pure $ IR.ESeq s (IR.Const 0)
unEx (Cx genstm) = do
  r <- U.newTemp
  t <- U.newLabel
  f <- U.newLabel
  pure $ IR.ESeq (IR.seqStm [IR.Move (IR.Temp r) (IR.Const 1), genstm t f, IR.Label f, IR.Move (IR.Temp r) (IR.Const 0), IR.Label t]) (IR.Temp r)

unNx :: Lookup xs "label" U.UniqueEff => Exp -> Eff xs IR.Stm
unNx (Ex e) = pure $ IR.Exp e
unNx (Nx s) = pure s
unNx (Cx genstm) = genstm <$> U.newLabel <*> U.newLabel

unCx :: Exp -> U.Label -> U.Label -> IR.Stm
unCx (Ex e) = undefined
unCx (Nx _) = undefined
unCx (Cx genstm) = genstm

simpleVarExp :: forall f xs. (F.Frame f, Lookup xs "nestingLevel" (NestingLevelEff f)) => Access f -> Eff xs (Maybe Exp)
simpleVarExp (Access level access) = fmap (Ex . F.exp (Proxy :: Proxy f) access) <$> pullInStaticLinksEff level

data VarEntry f = Var (Access f)

type VEnv f = E.Env (VarEntry f)
data TranslateError =
    VariableUndefined Id
  | VarialbeNotInScope Id
lookupVarAccess :: (
    Lookup xs "varEnv" (State (VEnv f))
  , Lookup xs "translateError"(EitherEff (RealLocated TranslateError))) => LId -> Eff xs (Access f)
lookupVarAccess (L loc id) =
   getsEff #varEnv (E.lookup id) >>= \case
    Nothing -> throwEff #translateError . L loc $ VariableUndefined id
    Just (Var access) -> pure access

transVar :: (Lookup xs "nestingLevel" (NestingLevelEff f), Lookup xs "varEnv" (State (VEnv f)), Lookup xs "translateError" (EitherEff (RealLocated TranslateError)), F.Frame f) => T.LValue -> Eff xs Exp
transVar (L loc (T.Id lid)) =
  lookupVarAccess lid >>= simpleVarExp >>= \case
    Just v -> pure v
    Nothing -> throwEff #translateError . L loc $ VarialbeNotInScope (unLId lid)
transExp :: (Lookup xs "nestingLevel" (NestingLevelEff f), Lookup xs "varEnv" (State (VEnv f)), Lookup xs "translateError"(EitherEff (RealLocated TranslateError)),  F.Frame f) =>  T.LExp -> Eff xs Exp
transExp (L _ (T.Var v)) = transVar v
