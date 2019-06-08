module Tiger.Semant where

import RIO
import qualified RIO.List as List
import qualified RIO.Map as Map
import qualified RIO.Set as Set

import Control.Monad.Cont
import Control.Monad.State (modify, gets)
import Control.Monad.Except
import Control.Lens ((.~))
import Data.Extensible
import Data.Extensible.Effect.Default

import qualified Env as E
import Id
import SrcLoc
import qualified Tiger.LSyntax as T
import UniqueID


data Type = TNil 
          | TUnit 
          | TInt 
          | TString 
          | TRecord (Record '["map" :> Map.Map Id Type, "id" :> Unique]) 
          | TArray (Record '["range" :> Type, "id" :> Unique]) 
          | TName LId
          deriving (Show)
data Var = Var Type 
         | Fun (Record '["domains" :> [Type], "codomain" :> Type])
         deriving (Show)
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

type Typing a = Eff '["type" >: State TEnv, "var" >: State VEnv, UniqueIDEff, EitherDef (RealLocated TypingError)] a
data TypingError = VariableUndefined Id
                 | VariableMismatchedWithDeclaredType Id Type Type
                 | TypeUndefined Id
                 | ExpectedType T.LExp Type Type
                 | ExpectedTypes [T.LExp] [Type] [Type]
                 | ExpectedUnitType T.LExp Type
                 | ExpectedIntType T.LExp Type
                 | ExpectedRecordType T.LValue Type
                 | ExpectedArrayType T.LValue Type
                 | MissingRecordField T.LValue Type Id
                 | InvalidRecTypeDeclaration [T.LDec]
                 | MultiDeclaredName [T.LDec]
                 | AssignNilWithNoRecordAnnotation
                 | NotImplemented String
instance Show TypingError where
  show (VariableUndefined id) = "undefined variable: " ++ show id
  show (VariableMismatchedWithDeclaredType id ty ty') = concat ["Couldn't match type: expression doesn't match with declared type: id = ", show id, ", declared type: ", show ty, ", actual type: ", show ty']
  show (TypeUndefined id) = "undefined type: " ++ show id
  show (ExpectedType (L _ e) ty ty') = concat ["Couldn't mach type: ", show ty, " type expected: exp = ", show e, ", actual type = ", show ty']
  show (ExpectedTypes es types types') = concat [
    "Couldn't match types: ", show types, " exptected: exps = ", show es, ", actual types = ", show types']
  show (ExpectedUnitType (L _ e) ty) = concat ["Couldn't match type: unit type expected: exp = ", show e, ", actual type: ", show ty]
  show (ExpectedIntType (L _ e) ty) = concat ["Couldn't match type: int type expected: exp = ", show e, ", actual type: ", show ty]
  show (ExpectedRecordType (L _ v) ty) = concat ["Couldn't match type: record type expected: value = ", show v, ", actual type: ", show ty]
  show (ExpectedArrayType (L _ v) ty) = concat ["Couldn't match type: array type expected: value = ", show v, ", actual type: ", show ty]
  show (MissingRecordField (L _ v) ty id) = concat ["Record field missing: value = ", show v, ", type = ", show ty, ", field = ", show id]
  show (InvalidRecTypeDeclaration decs) = concat ["Found circle type declarations: decs = ", show decs]
  show (MultiDeclaredName decs) = concat ["Same name types, vars or functions declared: decs = ", show decs]
  show AssignNilWithNoRecordAnnotation = "nil assigned with no type annotation"
  show (NotImplemented msg) = "not implemented: " ++ msg

runTyping :: Typing a -> Either (RealLocated TypingError) a
runTyping = leaveEff . runEitherDef . runUniqueIDEff . evalVEnvEff . evalTEnvEff


skipName :: Type -> Typing Type
skipName (TName id) = do
  ty <- lookupTypeId id
  skipName ty
skipName a@(TArray arr) = case arr ^. #range of
  TName id -> do
    ty <- lookupTypeId id >>= skipName
    return . TArray $ arr & #range .~ ty
  _ -> return a
skipName ty = return ty
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
lookupSkipName :: LId -> Typing Type
lookupSkipName = (skipName =<<) . lookupTypeId 
insertType :: Id -> Type -> Typing ()
insertType id ty = modifyEff #type $ E.insert id ty
insertVar :: Id -> Var -> Typing ()
insertVar id v = modifyEff #var $ E.insert id v
withTEnvScope :: Typing a -> Typing a
withTEnvScope = E.withEnvScope #type
withVEnvScope :: Typing a -> Typing a
withVEnvScope = E.withEnvScope #var

checkInt :: T.LExp -> Typing ()
checkInt e@(L loc _) = do
  ty <- typingExp e
  unless (ty == TInt) . throwError . L loc $ ExpectedIntType e ty
checkUnit :: T.LExp -> Typing ()
checkUnit e@(L loc _) = do
  ty <- typingExp e
  unless (ty == TUnit) . throwError . L loc $ ExpectedIntType e ty

type IRType = ((), Type)
typingExp :: T.LExp -> Typing Type
typingExp (L _ T.Nil) = return TNil
typingExp (L _ (T.Int _)) = return TInt
typingExp (L _ (T.String _)) = return TString
typingExp (L loc (T.ArrayCreate typeid size init)) = do
  ty <- lookupSkipName typeid
  case ty of
    TArray a -> do
      checkInt size
      initty <- typingExp init
      if initty == a ^. #range
        then return ty
        else throwError . L loc $ ExpectedType init (a ^. #range) initty
    _ -> throwError . L loc $ ExpectedArrayType (L loc (T.Id typeid)) ty
typingExp (L loc (T.RecordCreate typeid fields)) = do
  ty <- lookupTypeId typeid
  case ty of
    TRecord r -> do
      let m = Map.toList $ r ^. #map
      fieldsty <- mapM typingFieldAssign fields
      whenM (comp m fieldsty) $ throwError . L loc $ NotImplemented "1"
      return ty
    _ -> throwError . L loc $ ExpectedRecordType (L loc (T.Id typeid)) ty
  where
    comp [] [] = return True
    comp ((id, TName tyid):as) bs = do
      ty <- skipName (TName tyid)
      comp ((id, ty):as) bs
    comp as ((id, TName tyid):bs) = do
      ty <- skipName (TName tyid)
      comp as ((id, ty):bs)
    comp _ _ = return False
typingExp (L _ (T.Var v)) = typingValue v
typingExp (L loc (T.FunApply func args)) = do
  v <- lookupVarId func
  case v of
    Fun ty -> do
      argsty <- mapM typingExp args
      domains <- mapM skipName $ ty ^. #domains
      if argsty == domains
        then skipName $ ty ^. #codomain
        else throwError . L loc $ ExpectedTypes args domains argsty
    Var _ -> throwError . L loc $ NotImplemented "2"
typingExp (L loc (T.Op left (L _ op) right))
  | isEqNEq op = do
    leftty <- typingExp left
    rightty <- typingExp right
    if leftty <= rightty  || rightty <= leftty
      then return TInt
      else throwError . L loc $ ExpectedType right leftty rightty
  | otherwise = do
    checkInt left
    checkInt right
    return TInt
  where
    isEqNEq T.Eq = True
    isEqNEq T.NEq = True
    isEqNEq _ = False
typingExp (L _ (T.Seq exps)) = foldM (const typingExp) TUnit exps
typingExp (L loc (T.Assign var exp)) = do
  varty <- typingValue var
  expty <- typingExp exp
  if varty <= expty
    then return TUnit
    else throwError . L loc $ NotImplemented "3"
typingExp (L loc (T.If bool then' (Just else'))) = do
  checkInt bool
  thenty <- typingExp then'
  elsety <- typingExp else'
  if thenty == elsety
    then return thenty
    else throwError . L loc $ ExpectedType else' thenty elsety
typingExp (L loc (T.If bool then' Nothing)) = do
  checkInt bool
  thenty <- typingExp then'
  if thenty == TUnit
    then return TUnit
    else throwError . L loc $ ExpectedUnitType then' thenty
typingExp (L _ (T.While bool body)) = do
  checkInt bool
  checkUnit body
  return TUnit
typingExp (L loc (T.For (L _ id) from to body)) = do
  checkInt from
  checkInt to
  withTEnvScope $ do
    insertType id TInt
    bodyty <- typingExp body 
    if bodyty == TUnit
      then return TUnit
      else throwError . L loc $ NotImplemented "5"
typingExp (L _ T.Break) = return TUnit
typingExp (L loc (T.Let decs body)) = do
  checkSameNameDec loc decs
  let (typedecs, rest) = List.partition isTypeDec decs
  let typenames = map name typedecs
  checkInvalidRecType loc typedecs
  types <- withTEnvScope $ do
    mapM_ (\lid -> insertType (unLId lid) (TName lid)) typenames
    mapM (fmap (fromMaybe TNil) . typingDec) typedecs
  withTEnvScope $ do
    zipM_ (\lid ty -> insertType (unLId lid) ty) typenames types
    withVEnvScope $ do
      let fundecs = List.filter isFunDec rest
      mapM_ insertFunEntry fundecs
      mapM_ typingDec rest
      typingExp body
  where
    isTypeDec (L _ (T.TypeDec _ _)) = True
    isTypeDec _ = False
    isFunDec (L _ (T.FunDec _ _ _ _)) = True
    isFunDec _ = False
    name (L _ (T.TypeDec lid _)) = lid
    zipM_ _ [] _ = return ()
    zipM_ _ _ [] = return ()
    zipM_ f (a:as) (b:bs) = f a b >> zipM_ f as bs

checkSameNameDec :: RealSrcSpan -> [T.LDec] -> Typing ()
checkSameNameDec loc decs = unless (runCheckSameNameDec decs) . throwError . L loc $ MultiDeclaredName decs
  where
    runCheckSameNameDec = leaveEff . flip (evalStateEff @"func") Set.empty . flip (evalStateEff @"var") Set.empty . flip (evalStateEff @"type") Set.empty . checkSameNameDec'
checkSameNameDec' :: [T.LDec] -> Eff '["type" >: State (Set.Set Id), "var" >: State (Set.Set Id), "func" >: State (Set.Set Id)] Bool
checkSameNameDec' [] = return True
checkSameNameDec' (L loc (T.FunDec (L _ id) _ _ _):decs) = flip (runContEff @"Cont") return . callCC $ \exit -> do
  getsEff #func (Set.member id) >>= bool (return True) (exit False)
  getsEff #var (Set.member id) >>= bool (return True) (exit False)
  modifyEff #func (Set.insert id)
  castEff $ checkSameNameDec' decs
checkSameNameDec' (L _ (T.VarDec (L _ id) _ _):decs) = flip (runContEff @"Cont") return . callCC $ \exit -> do
  getsEff #func (Set.member id) >>= bool (return True) (exit False)
  modifyEff #var (Set.insert id)
  castEff $ checkSameNameDec' decs
checkSameNameDec' (L _ (T.TypeDec (L _ id) _):decs) = flip (runContEff @"Cont") return . callCC $ \exit -> do
  getsEff #type (Set.member id) >>= bool (return True) (exit False)
  modifyEff #type (Set.insert id)
  castEff $ checkSameNameDec' decs
checkInvalidRecType :: RealSrcSpan -> [T.LDec] -> Typing ()
checkInvalidRecType loc decs = do
  res <- mapM runCheckInvalidRecType ids
  if and res
    then return ()
    else throwError . L loc $ InvalidRecTypeDeclaration decs
  where
    idAndType (L _ (T.TypeDec lid ty)) = (unLId lid, ty)
    idtypes = map idAndType decs
    ids = map fst idtypes
    runCheckInvalidRecType = flip  evalStateDef Set.empty . flip runReaderDef (Map.fromList idtypes) . checkInvalidRecType'
checkInvalidRecType' :: Id -> Eff '[ReaderDef (Map.Map Id T.LType), StateDef (Set Id), "type" >: State TEnv, "var" >: State VEnv, UniqueIDEff, EitherDef (RealLocated TypingError)] Bool
checkInvalidRecType' id = flip (runContEff @"Cont") return . callCC $ \exit -> do
  gets (Set.member id) >>= bool (return True) (exit False)
  decs <- ask
  case decs Map.!? id of
    Just (L _ (T.TypeId (L _ id'))) -> do
      getsEff #type (isJust . E.lookup id') >>= bool (return True) (exit True)
      modify (Set.insert id)
      castEff $ checkInvalidRecType' id'
    _ -> return True

typingValue :: T.LValue -> Typing Type
typingValue (L loc (T.Id id)) = do
  var <- lookupVarId id
  case var of
    Var ty -> return ty -- neccesary to consider the case of NAME type
    Fun ty -> throwError . L loc $ NotImplemented "6"
typingValue (L loc (T.RecField v (L _ field))) = do
  ty <- typingValue v
  case ty of
    TRecord r -> case Map.lookup field (r ^. #map) of
      Just ty -> return ty
      Nothing -> throwError . L loc $ MissingRecordField v ty field
    _ -> throwError . L loc $ ExpectedRecordType v ty
typingValue (L loc (T.ArrayIndex v e)) = do
  ty <- typingValue v >>= skipName
  case ty of
    TArray a -> do
      checkInt e
      return $ a ^. #range
    _ -> throwError . L loc $ ExpectedArrayType v ty

typingFieldAssign :: T.LFieldAssign -> Typing (Id, Type)
typingFieldAssign (L _ (T.FieldAssign (L _ id) e)) = (id,) <$> typingExp e

insertFunEntry :: T.LDec -> Typing ()
insertFunEntry (L _ (T.FunDec (L _ id) args ret _)) = insertVar id . Fun $ #domains @= map (TName . typeid) args <: #codomain @= retty <: nil
  where
    typeid (L _ (T.Field _ id)) = id
    retty = case ret of
      Just retid -> TName retid
      Nothing -> TUnit

typingDec :: T.LDec -> Typing (Maybe Type)
typingDec (L loc (T.FunDec _ args ret body)) = do
  argsty <- mapM typingField args 
  retty <- maybe (return TUnit) lookupTypeId ret
  withVEnvScope $ do
    mapM_ (\(id, ty) -> insertVar id (Var ty)) argsty
    bodyty <- typingExp body
    if bodyty == retty
      then return Nothing
      else throwError . L loc $ ExpectedType body retty bodyty
typingDec (L loc (T.VarDec (L _ id) (Just typeid) e)) = do
  ty <- lookupTypeId typeid >>= skipName
  ty' <- typingExp e
  if ty <= ty' -- opposite to subtyping
    then modifyEff #var (E.insert id (Var ty)) >> return Nothing
    else throwError . L loc $ VariableMismatchedWithDeclaredType id ty ty'
typingDec (L loc (T.VarDec (L _ id) Nothing e)) = do
  t <- typingExp e
  when (t == TNil) . throwError . L loc $ AssignNilWithNoRecordAnnotation
  modifyEff #var $ E.insert id (Var t)
  return Nothing
typingDec (L _ (T.TypeDec _ ty)) = Just <$> typingType ty

typingType :: T.LType -> Typing Type
typingType (L _ (T.TypeId typeid)) = lookupTypeId typeid
typingType (L _ (T.RecordType fields)) = do
  fieldmap <- foldM (\e field -> (\(id, ty) -> Map.insert id ty e) <$> typingField field) Map.empty fields
  id <- getIDEff
  return . TRecord $ #map @= fieldmap <: #id @= id <: nil
typingType (L _ (T.ArrayType typeid)) = do
  ty <- lookupTypeId typeid
  id <- getIDEff
  return . TArray $ #range @= ty <: #id @= id <: nil

typingField :: T.LField -> Typing (Id, Type)
typingField (L _ (T.Field (L _ id) typeid)) = (id,) <$> lookupTypeId typeid 
