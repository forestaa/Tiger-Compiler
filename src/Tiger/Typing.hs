module Tiger.Typing where

-- import Control.Monad
import Control.Monad.Except
-- import Data.Extensible
-- import Data.Extensible.Effect


import RIO
import qualified RIO.Map as Map
import Data.Extensible
import Data.Extensible.Effect.Default

import qualified Env as E
import Id
import SrcLoc
import qualified Tiger.LSyntax as T

newtype Unique = Unique Int deriving (Eq, Show)
data Type = TNil 
          | TUnit 
          | TInt 
          | TString 
          | TRecord (Record '["map" :> Map.Map Id Type, "id" :> Unique]) 
          | TArray (Record '["range" :> Type, "id" :> Unique]) 
          | TName (Record '["name" :> Id, "type" :> Maybe Type])
          deriving (Show)
data Var = Var Type 
         | Fun (Record '["domains" :> [Type], "codomain" :> Type])
instance Eq Type where
  TNil == TNil = True
  TUnit == TUnit = True
  TInt == TInt = True
  TString == TString = True
  (TRecord r) == (TRecord r') = r ^. #id == r' ^. #id
  (TArray a) == (TArray a') = a ^. #id == a' ^. #id
  _ == _ = False
instance Ord Type where
  (TRecord _) <= TNil = True
  ty <= ty' = ty == ty'


type TEnv = E.Env Type
initTEnv :: TEnv
initTEnv = foldr (uncurry E.insert) E.empty [("string", TString), ("int", TInt)]
evalTEnvEff :: Eff (("type" >: State TEnv) ': xs) a -> Eff xs a
evalTEnvEff = flip (evalStateEff @"type") initTEnv
type VEnv = E.Env Var
initVEnv :: VEnv
initVEnv = foldr (uncurry E.insert) E.empty []
evalVEnvEff :: Eff (("var" >: State VEnv) ': xs) a -> Eff xs a
evalVEnvEff = flip (evalStateEff @"var") initVEnv

type Typing a = Eff '["type" >: State TEnv, "var" >: State VEnv, "id" >: State Int, EitherDef (RealLocated TypingError)] a
data TypingError = VariableUndefined Id
                 | VariableMismatchedWithDeclaredType Id Type Type
                 | TypeUndefined Id
                 | ExpectedType T.LExp Type Type
                 | ExpectedIntType T.LExp Type
                 | ExpectedRecordType T.LValue Type
                 | ExpectedArrayType T.LValue Type
                 | MissingRecordField T.LValue Type Id
                 | NotImplemented
instance Show TypingError where
  show (VariableUndefined id) = "undefined variable: " ++ show id
  show (VariableMismatchedWithDeclaredType id ty ty') = concat ["Couldn't match type: expression doesn't match with declared type: id = ", show id, ", declared type", show ty, ", actual type: ", show ty']
  show (TypeUndefined id) = "undefined type: " ++ show id
  show (ExpectedType (L _ e) ty ty') = concat ["Couldn't mach type: ", show ty, " type expected: exp = ", show e, ", actual type = ", show ty']
  show (ExpectedIntType (L _ e) ty) = concat ["Couldn't match type: int type expected: exp = ", show e, ", actual type: ", show ty]
  show (ExpectedRecordType (L _ v) ty) = concat ["Couldn't match type: record type expected: value = ", show v, ", actual type: ", show ty]
  show (ExpectedArrayType (L _ v) ty) = concat ["Couldn't match type: array type expected: value = ", show v, ", actual type: ", show ty]
  show (MissingRecordField (L _ v) ty id) = concat ["Record field missing: value = ", show v, ", type = ", show ty, ", field = ", show id]
  show NotImplemented = "not implemented"

runTyping :: Typing a -> Either (RealLocated TypingError) a
runTyping = leaveEff . runEitherDef . flip (evalStateEff @"id") 0 . evalVEnvEff . evalTEnvEff

lookupTypeId :: LId -> Typing Type
lookupTypeId (L loc id) = do
  m <- E.lookup id <$> getEff #type
  case m of
    Nothing -> throwError . L loc $ TypeUndefined id
    Just ty -> return ty
lookupVarId :: LId -> Typing Var
lookupVarId (L loc id) = do
  m <- E.lookup id <$> getEff #var
  case m of
    Nothing -> throwError . L loc $ VariableUndefined id
    Just v -> return v
getid :: Typing Unique
getid = do
  id <- getEff #id
  putEff #id (id + 1)
  return $ Unique id


type IRType = ((), Type)
typingExp :: T.LExp -> Typing Type
typingExp (L _ T.Nil) = return TNil
typingExp (L _ (T.Int _)) = return TInt
typingExp (L _ (T.String _)) = return TString
typingExp (L loc (T.ArrayCreate typeid size init)) = do
  ty <- lookupTypeId typeid
  case ty of
    TArray a -> do
      sizety <- typingExp size
      initty <- typingExp init
      case sizety of
        TInt |  initty == a ^. #range -> return ty
        TInt -> throwError . L loc $ ExpectedType init (a ^. #range) initty
        _ -> throwError . L loc $ ExpectedIntType size initty
    _ -> throwError . L loc $ ExpectedArrayType (L loc (T.Id typeid)) ty
typingExp (L loc (T.RecordCreate typeid fields)) = do
  ty <- lookupTypeId typeid
  case ty of
    TRecord r -> do
      fieldsty <- foldM (\e fa -> (\(id, ty) -> Map.insert id ty e) <$> typingFieldAssign fa) Map.empty fields
      if r ^. #map <= fieldsty
        then return ty
        else throwError . L loc $ NotImplemented
    _ -> throwError . L loc $ ExpectedRecordType (L loc (T.Id typeid)) ty
typingExp (L _ (T.Var v)) = typingValue v
typingExp (L loc (T.FunApply func args)) = do
  v <- lookupVarId func
  case v of
    Fun ty -> do
      argsty <- mapM typingExp args
      if argsty == ty ^. #domains
        then return $ ty ^. #codomain
        else throwError . L loc $ NotImplemented
    Var _ -> throwError . L loc $ NotImplemented
typingExp (L loc (T.Op left _ right)) = do
  lty <- typingExp left
  rty <- typingExp right
  case (lty, rty) of
    (TInt, TInt) -> return TInt
    _ -> throwError . L loc $ NotImplemented
typingExp (L _ (T.Seq exps)) = foldM (const typingExp) TUnit exps
typingExp (L loc (T.Assign var exp)) = do
  varty <- typingValue var
  expty <- typingExp exp
  if varty == expty
    then return TUnit
    else throwError . L loc $ NotImplemented
typingExp (L loc (T.If bool then' m)) = do
  boolty <- typingExp bool
  if boolty == TInt
    then do
      thenty <- typingExp then'
      case m of
        Just else' -> do
          elsety <- typingExp else'
          if thenty == elsety
            then return thenty
            else throwError . L loc $ NotImplemented
        Nothing 
          | thenty == TUnit -> return TUnit
          | otherwise -> throwError . L loc $ NotImplemented
    else throwError . L loc $ NotImplemented
typingExp (L loc (T.While bool body)) = do
  boolty <- typingExp bool
  bodyty <- typingExp body
  if boolty == TInt && bodyty == TUnit
    then return TUnit
    else throwError . L loc $ NotImplemented
typingExp (L loc (T.For (L _ id) from to body)) = do
  fromty <- typingExp from
  toty <- typingExp to
  if fromty == TInt && toty == TInt
    then do
      modifyEff #type E.beginScope
      modifyEff #type $ E.insert id TInt
      bodyty <- typingExp body
      modifyEff #type E.endScope
      if bodyty == TUnit
        then return TUnit
        else throwError . L loc $ NotImplemented
    else throwError . L loc $ NotImplemented
typingExp (L _ T.Break) = return TUnit
typingExp (L _ (T.Let decs body)) = do
  modifyEff #type E.beginScope
  modifyEff #var E.beginScope
  mapM_ typingDec decs
  bodyty <- typingExp body
  modifyEff #var E.endScope
  modifyEff #type E.endScope
  return bodyty

typingValue :: T.LValue -> Typing Type
typingValue (L loc (T.Id id)) = do
  var <- lookupVarId id
  case var of
    Var ty -> return ty -- neccesary to consider the case of NAME type
    Fun ty -> throwError . L loc $ NotImplemented
typingValue (L loc (T.RecField v (L _ field))) = do
  ty <- typingValue v
  case ty of
    TRecord r -> case Map.lookup field (r ^. #map) of
      Just ty -> return ty
      Nothing -> throwError . L loc $ MissingRecordField v ty field
    _ -> throwError . L loc $ ExpectedRecordType v ty
typingValue (L loc (T.ArrayIndex v e)) = do
  ty <- typingValue v
  case ty of
    TArray a -> do
      ty' <- typingExp e
      if ty' == TInt
        then return $ a ^. #range
        else throwError . L loc $ ExpectedIntType e ty'
    _ -> throwError . L loc $ ExpectedArrayType v ty

typingFieldAssign :: T.LFieldAssign -> Typing (Id, Type)
typingFieldAssign (L _ (T.FieldAssign (L _ id) e)) = (id,) <$> typingExp e

typingDec :: T.LDec -> Typing ()
typingDec (L loc (T.FunDec (L _ id) args (Just retid) body)) = do
  argsty <- mapM typingField args 
  retty <- lookupTypeId retid
  modifyEff #var (E.insert id . Fun $ #domains @= snd <$> argsty <: #codomain @= retty <: nil)
  modifyEff #var E.beginScope
  modifyEff #var (\e -> foldr (\(id, ty) -> E.insert id (Var ty)) e argsty)
  bodyty <- typingExp body
  if bodyty == retty
    then modifyEff #var E.endScope >> return ()
    else throwError . L loc $ NotImplemented
typingDec (L loc (T.VarDec (L _ id) (Just typeid) e)) = do
  ty <- lookupTypeId typeid
  ty' <- typingExp e
  if ty <= ty' -- opposite to subtyping
    then modifyEff #var $ E.insert id (Var ty)
    else throwError . L loc $ VariableMismatchedWithDeclaredType id ty ty'
typingDec (L _ (T.VarDec (L _ id) Nothing e)) = do
  t <- typingExp e
  modifyEff #var $ E.insert id (Var t)
typingDec (L _ (T.TypeDec (L _ id) ty)) = do
  ty' <- typingType ty
  modifyEff #type $ E.insert id ty'

typingType :: T.LType -> Typing Type
typingType (L _ (T.TypeId typeid)) = lookupTypeId typeid
typingType (L _ (T.RecordType fields)) = do
  fieldmap <- foldM (\e field -> (\(id, ty) -> Map.insert id ty e) <$> typingField field) Map.empty fields
  id <- getid
  return . TRecord $ #map @= fieldmap <: #id @= id <: nil
typingType (L _ (T.ArrayType typeid)) = do
  ty <- lookupTypeId typeid
  id <- getid
  return . TArray $ #range @= ty <: #id @= id <: nil

typingField :: T.LField -> Typing (Id, Type)
typingField (L _ (T.Field (L _ id) typeid)) = (id,) <$> lookupTypeId typeid
