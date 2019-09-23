module Tiger.Semant where

import RIO
import qualified RIO.Partial as Partial
import qualified RIO.List as List
import qualified RIO.Map as Map
import qualified RIO.Set as Set

import Control.Monad.State (modify, gets)
import Control.Monad.Except
import Control.Lens ((.~))
import Data.Extensible
import Data.Extensible.Effect.Default

import qualified Env as E
import qualified Frame as F
import Id
import SrcLoc
import qualified Tiger.LSyntax as T
import Unique
import Tiger.Semant.Types
import Tiger.Semant.Translate


initTEnv :: TEnv
initTEnv = E.fromList [("string", TString), ("int", TInt)]
initVEnv :: VEnv f
initVEnv = E.fromList [
  ("print", Fun $ #domains @= [TString] <: #codomain @= TUnit <: nil),
  ("flush", Fun $ #domains @= [] <: #codomain @= TUnit <: nil),
  ("getchar", Fun $ #domains @= [] <: #codomain @= TString <: nil),
  ("ord", Fun $ #domains @= [TString] <: #codomain @= TInt <: nil),
  ("chr", Fun $ #domains @= [TInt] <: #codomain @= TString <: nil),
  ("size", Fun $ #domains @= [TString] <: #codomain @= TInt <: nil),
  ("substring", Fun $ #domains @= [TString, TInt, TInt] <: #codomain @= TString <: nil),
  ("concat", Fun $ #domains @= [TString, TString] <: #codomain @= TString <: nil),
  ("not", Fun $ #domains @= [TInt] <: #codomain @= TInt <: nil),
  ("exit", Fun $ #domains @= [TInt] <: #codomain @= TUnit <: nil)
  ]

data TranslateError =
  -- typing
    VariableUndefined Id
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

  -- translate
  | VariableNotInScope Id

  | NotImplemented String
instance Show TranslateError where
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

  show (VariableNotInScope id) = "variable not in scope: " ++ show id

  show (NotImplemented msg) = "not implemented: " ++ msg


type HasTranslateEff xs f = (F.Frame f, HasEnv xs f, Lookup xs "translateError" (EitherEff (RealLocated TranslateError)), Lookup xs "nestingLevel" (NestingLevelEff f), Lookup xs "label" UniqueEff)
runTranslateEff ::
     Eff (
         ("typeEnv" >: State TEnv)
      ': ("varEnv" >: State (VEnv f))
      ': ("nestingLevel" >: NestingLevelEff f)
      ': ("temp" >: UniqueEff)
      ': ("label" >: UniqueEff)
      ': ("translateError" >: EitherEff (RealLocated TranslateError))
      ': xs) a
  -> Eff xs (Either (RealLocated TranslateError) a)
runTranslateEff = runEitherEff @"translateError" . runUniqueEff @"label" . runUniqueEff @"temp" . runNestingLevelEff . evalEnvEff initTEnv initVEnv


lookupTypeIdEff :: (Lookup xs "typeEnv" (State TEnv), Lookup xs "translateError" (EitherEff (RealLocated TranslateError))) => LId -> Eff xs Type
lookupTypeIdEff (L loc id) = lookupTypeId id >>= maybe (throwEff #translateError . L loc $ TypeUndefined id) pure
lookupVarIdEff ::  (
    Lookup xs "varEnv" (State (VEnv f))
  , Lookup xs "translateError" (EitherEff (RealLocated TranslateError))
  ) => LId -> Eff xs (Var f)
lookupVarIdEff (L loc id) = lookupVarId id >>= maybe (throwEff #translateError . L loc $ VariableUndefined id) pure

skipName :: (
    Lookup xs "typeEnv" (State TEnv)
  , Lookup xs "translateError" (EitherEff (RealLocated TranslateError))
  ) => Type -> Eff xs Type
skipName (TName lid) = lookupTypeIdEff lid >>= skipName
skipName a@(TArray arr) = case arr ^. #range of
  TName id -> do
    -- ty <-skipName =<< lookupTypeId id
    ty <- skipName (TName id)
    pure . TArray $ arr & #range .~ ty
  _ -> pure a
skipName ty = pure ty
-- lookupTypeId :: (
--     Lookup xs "typeEnv" (State TEnv)
--   , Lookup xs "translateError" (EitherEff (RealLocated TranslateError))
--   ) => LId -> Eff xs Type
-- lookupTypeId (L loc id) = do
--   m <- getsEff #typeEnv $ E.lookup id
--   case m of
--     Nothing -> throwEff #translateError . L loc $ TypeUndefined id
--     Just ty -> pure ty
-- lookupVarId :: (
--     Lookup xs "varEnv" (State VEnv)
--   , Lookup xs "translateError" (EitherEff (RealLocated TranslateError))
--   ) => LId -> Eff xs Var
-- lookupVarId (L loc id) = do
--   m <- getsEff #varEnv $ E.lookup id
--   case m of
--     Nothing -> throwEff #translateError . L loc $ VariableUndefined id
--     Just v -> pure v
lookupSkipName :: (
    Lookup xs "typeEnv" (State TEnv)
  , Lookup xs "translateError" (EitherEff (RealLocated TranslateError))
  ) => LId -> Eff xs Type
lookupSkipName = skipName <=< lookupTypeIdEff
-- insertType :: (Lookup xs "typeEnv" (State TEnv)) => Id -> Type -> Eff xs ()
-- insertType id ty = modifyEff #typeEnv $ E.insert id ty
-- insertVar :: (Lookup xs "varEnv" (State VEnv)) => Id -> Var -> Eff xs ()
-- insertVar id v = modifyEff #varEnv $ E.insert id v
-- withTEnvScope :: (Lookup xs "typeEnv" (State TEnv)) => Eff xs a -> Eff xs a
-- withTEnvScope = E.withEnvScope #typeEnv
-- withVEnvScope :: (Lookup xs "varEnv" (State VEnv)) => Eff xs a -> Eff xs a
-- withVEnvScope = E.withEnvScope #varEnv

checkInt :: (Lookup xs "translateError" (EitherEff (RealLocated TranslateError))) => Type -> T.LExp -> Eff xs ()
checkInt ty e@(L loc _) =
  unless (ty == TInt) . throwEff #translateError . L loc $ ExpectedIntType e ty
-- checkUnit :: HasTypingEff xs f => T.LExp -> Eff xs ()
-- checkUnit e@(L loc _) = do
--   ty <- typingExp e
--   unless (ty == TUnit) . throwEff #translateError . L loc $ ExpectedIntType e ty

translateExp :: HasTranslateEff xs f => T.LExp -> Eff xs (Exp, Type)
translateExp (L _ (T.Var v)) = translateValue v
translateExp e@(L loc (T.Op left (L _ op) right)) = translateBinOp e

translateValue :: forall f xs. (HasTranslateEff xs f) => T.LValue -> Eff xs (Exp, Type)
translateValue (L loc (T.Id lid)) = do
  var <- lookupVarIdEff lid
  case var of
    Var r -> valueIdExp (r ^. #access) >>= maybe (throwEff #translateError . L loc $ VariableNotInScope (unLId lid)) (pure . (, r ^. #type))
    Fun _ -> throwEff #translateError . L loc $ NotImplemented "6"
translateValue (L loc (T.RecField lv (L _ field))) = do
  (varExp, ty) <- translateValue lv
  case ty of
    TRecord r -> case Map.lookup field (r ^. #map) of
      Just ty -> pure . (, ty) $ valueRecFieldExp @f varExp (Partial.fromJust (Map.lookupIndex field (r ^. #map)))
      Nothing -> throwEff #translateError . L loc $ MissingRecordField lv ty field
    _ -> throwEff #translateError . L loc $ ExpectedRecordType lv ty
translateValue (L loc (T.ArrayIndex lv le)) =
  translateValue lv >>= traverse skipName >>= \case
    (varExp, TArray a) -> do
      (indexExp, indexTy) <- translateExp le
      checkInt indexTy le
      pure . (, a ^. #range) $ valueArrayIndexExp @f varExp indexExp
    (_, ty) -> throwEff #translateError . L loc $ ExpectedArrayType lv ty

translateBinOp :: HasTranslateEff xs f => T.LOp -> Eff xs (Exp, Type)
translateBinOp (L loc (T.Op left (L _ op) right)) = do
  (leftExp, leftTy) <- translateExp left
  (rightExp, rightTy) <- translateExp right
  typecheck op leftTy left rightTy right
  pure . (, TInt) $ binOpExp op leftExp rightExp
  where
    isEqNEq T.Eq = True
    isEqNEq T.NEq = True
    isEqNEq _ = False
    isComparable leftTy rightTy = leftTy <= rightTy || rightTy <= leftTy

    typecheck op leftTy left rightTy right
      | not (isEqNEq op) = checkInt leftTy left >> checkInt rightTy right
      | isEqNEq op && not (isComparable leftTy rightTy) = throwEff #translateError . L loc $ ExpectedType right leftTy rightTy
      | otherwise = pure ()



-- typingExp :: HasTypingEff xs f => T.LExp -> Eff xs Type
-- typingExp (L _ T.Nil) = pure TNil
-- typingExp (L _ (T.Int _)) = pure TInt
-- typingExp (L _ (T.String _)) = pure TString
-- typingExp (L loc (T.ArrayCreate typeid size init)) = do
--   ty <- lookupSkipName typeid
--   case ty of
--     TArray a -> do
--       checkInt size
--       initty <- typingExp init
--       if initty == a ^. #range
--         then pure ty
--         else throwEff #translateError . L loc $ ExpectedType init (a ^. #range) initty
--     _ -> throwEff #translateError . L loc $ ExpectedArrayType (L loc (T.Id typeid)) ty
-- typingExp (L loc (T.RecordCreate typeid fields)) = do
--   ty <- lookupTypeIdEff typeid
--   case ty of
--     TRecord r -> do
--       let m = Map.toList $ r ^. #map
--       fieldsty <- mapM typingFieldAssign fields
--       whenM (comp m fieldsty) $ throwEff #translateError . L loc $ NotImplemented "1"
--       pure ty
--     _ -> throwEff #translateError . L loc $ ExpectedRecordType (L loc (T.Id typeid)) ty
--   where
--     comp :: (Lookup xs "typeEnv" (State TEnv), Lookup xs "translateError" (EitherEff (RealLocated TranslateError))) => [(Id, Type)] -> [(Id, Type)] -> Eff xs Bool
--     comp [] [] = pure True
--     comp ((id, TName tyid):as) bs = do
--       ty <- skipName (TName tyid)
--       comp ((id, ty):as) bs
--     comp as ((id, TName tyid):bs) = do
--       ty <- skipName (TName tyid)
--       comp as ((id, ty):bs)
--     comp _ _ = pure False
-- typingExp (L _ (T.Var v)) = typingValue v
-- typingExp (L loc (T.FunApply func args)) = do
--   v <- lookupVarIdEff func
--   case v of
--     Fun ty -> do
--       argsty <- mapM (skipName <=< typingExp) args
--       domains <- mapM skipName $ ty ^. #domains
--       if argsty == domains
--         then skipName $ ty ^. #codomain
--         else throwEff #translateError . L loc $ ExpectedTypes args domains argsty
--     Var _ -> throwEff #translateError . L loc $ NotImplemented "2"
-- typingExp (L loc (T.Op left (L _ op) right))
--   | isEqNEq op = do
--     leftty <- typingExp left
--     rightty <- typingExp right
--     if leftty <= rightty  || rightty <= leftty
--       then pure TInt
--       else throwEff #translateError . L loc $ ExpectedType right leftty rightty
--   | otherwise = do
--     checkInt left
--     checkInt right
--     pure TInt
--   where
--     isEqNEq T.Eq = True
--     isEqNEq T.NEq = True
--     isEqNEq _ = False
-- typingExp (L _ (T.Seq exps)) = foldM (const typingExp) TUnit exps
-- typingExp (L loc (T.Assign var exp)) = do
--   varty <- typingValue var
--   expty <- typingExp exp
--   if varty <= expty
--     then pure TUnit
--     else throwEff #translateError . L loc $ NotImplemented "3"
-- typingExp (L loc (T.If bool then' (Just else'))) = do
--   checkInt bool
--   thenty <- typingExp then'
--   elsety <- typingExp else'
--   if thenty <= elsety || elsety <= thenty
--     then pure thenty
--     else throwEff #translateError . L loc $ ExpectedType else' thenty elsety
-- typingExp (L loc (T.If bool then' Nothing)) = do
--   checkInt bool
--   thenty <- typingExp then'
--   if thenty == TUnit
--     then pure TUnit
--     else throwEff #translateError . L loc $ ExpectedUnitType then' thenty
-- typingExp (L _ (T.While bool body)) = do
--   checkInt bool
--   checkUnit body
--   pure TUnit
-- typingExp (L loc (T.For (L _ id) _ from to body)) = do
--   checkInt from
--   checkInt to
--   withTEnvScope $ do
--     insertVar id $ Var TInt
--     bodyty <- typingExp body
--     if bodyty == TUnit
--       then pure TUnit
--       else throwEff #translateError . L loc $ NotImplemented "5"
-- typingExp (L _ T.Break) = pure TUnit
-- typingExp (L loc (T.Let decs body)) = do
--   checkSameNameDec loc decs
--   let (typedecs, rest) = List.partition isTypeDec decs
--   let typenames = map name typedecs
--   getEff #typeEnv >>= checkInvalidRecType loc typedecs
--   types <- withTEnvScope $ do
--     mapM_ (\lid -> insertType (unLId lid) (TName lid)) typenames
--     mapM (fmap (fromMaybe TNil) . typingDec) typedecs
--   withTEnvScope $ do
--     zipM_ (\lid ty -> insertType (unLId lid) ty) typenames types
--     withVEnvScope $ do
--       let fundecs = List.filter isFunDec rest
--       mapM_ insertFunEntry fundecs
--       mapM_ typingDec rest
--       typingExp body
--   where
--     isTypeDec (L _ (T.TypeDec _ _)) = True
--     isTypeDec _ = False
--     isFunDec (L _ (T.FunDec _ _ _ _)) = True
--     isFunDec _ = False
--     name (L _ (T.TypeDec lid _)) = lid
--     zipM_ _ [] _ = pure ()
--     zipM_ _ _ [] = pure ()
--     zipM_ f (a:as) (b:bs) = f a b >> zipM_ f as bs

-- checkSameNameDec :: Lookup xs "translateError" (EitherEff (RealLocated TranslateError)) => RealSrcSpan -> [T.LDec] -> Eff xs ()
-- checkSameNameDec loc decs = unless (runCheckSameNameDec decs) . throwEff #translateError . L loc $ MultiDeclaredName decs
--   where
--     runCheckSameNameDec = leaveEff . flip (evalStateEff @"func") Set.empty . flip (evalStateEff @"var") Set.empty . flip (evalStateEff @"type") Set.empty . checkSameNameDec'

--     checkSameNameDec' [] = pure True
--     checkSameNameDec' (L _ (T.FunDec (L _ id) _ _ _):decs) = flip (runContEff @"cont") pure $ do
--       getsEff #func (Set.member id) >>= bool (pure True) (contEff #cont $ const (pure False))
--       getsEff #var (Set.member id) >>= bool (pure True) (contEff #cont $ const (pure False))
--       modifyEff #func (Set.insert id)
--       castEff $ checkSameNameDec' decs
--     checkSameNameDec' (L _ (T.VarDec (L _ id) _ _ _):decs) = flip (runContEff @"cont") pure $ do
--       getsEff #func (Set.member id) >>= bool (pure True) (contEff #cont $ const (pure False))
--       modifyEff #var (Set.insert id)
--       castEff $ checkSameNameDec' decs
--     checkSameNameDec' (L _ (T.TypeDec (L _ id) _):decs) = flip (runContEff @"cont") pure $ do
--       getsEff #type (Set.member id) >>= bool (pure True) (contEff #cont $ const (pure False))
--       modifyEff #type (Set.insert id)
--       castEff $ checkSameNameDec' decs


-- checkInvalidRecType :: (Lookup xs "translateError" (EitherEff (RealLocated TranslateError))) => RealSrcSpan -> [T.LDec] -> TEnv -> Eff xs ()
-- checkInvalidRecType loc decs tenv =
--   if and $ fmap runCheckInvalidRecType ids
--     then pure ()
--     else throwEff #translateError . L loc $ InvalidRecTypeDeclaration decs
--   where
--     idAndType (L _ (T.TypeDec lid ty)) = (unLId lid, ty)
--     idtypes = map idAndType decs
--     ids = map fst idtypes
--     runCheckInvalidRecType = leaveEff . flip  (evalStateEff @"typeIds") Set.empty . flip (runReaderEff @"typeIdToType") (Map.fromList idtypes) . flip (runReaderEff @"typeEnv") tenv . checkInvalidRecType'

--     checkInvalidRecType' id = flip (runContEff @"cont") pure $ do
--       getsEff #typeIds (Set.member id) >>= bool (pure True) (contEff #cont $ const (pure False))
--       decs <- askEff #typeIdToType
--       case decs Map.!? id of
--         Just (L _ (T.TypeId (L _ id'))) -> do
--           asksEff #typeEnv (isJust . E.lookup id') >>= bool (pure True) (contEff #cont $ const (pure True))
--           modifyEff #typeIds (Set.insert id)
--           castEff $ checkInvalidRecType' id'
--         _ -> pure True

-- typingValue :: HasTypingEff xs f => T.LValue -> Eff xs Type
-- typingValue (L loc (T.Id id)) = do
--   var <- lookupVarIdEff id
--   case var of
--     Var r -> pure $ r ^. #type -- neccesary to consider the case of NAME type
--     Fun _ -> throwEff #translateError . L loc $ NotImplemented "6"
-- typingValue (L loc (T.RecField v (L _ field))) = do
--   ty <- typingValue v
--   case ty of
--     TRecord r -> case Map.lookup field (r ^. #map) of
--       Just ty -> pure ty
--       Nothing -> throwEff #translateError . L loc $ MissingRecordField v ty field
--     _ -> throwEff #translateError . L loc $ ExpectedRecordType v ty
-- typingValue (L loc (T.ArrayIndex v e)) = do
--   ty <- typingValue v >>= skipName
--   case ty of
--     TArray a -> do
--       checkInt e
--       pure $ a ^. #range
--     _ -> throwEff #translateError . L loc $ ExpectedArrayType v ty

-- typingFieldAssign :: HasTypingEff xs f => T.LFieldAssign -> Eff xs (Id, Type)
-- typingFieldAssign (L _ (T.FieldAssign (L _ id) e)) = (id,) <$> typingExp e

-- insertFunEntry :: (Lookup xs "varEnv" (State (VEnv f))) => T.LDec -> Eff xs ()
-- insertFunEntry (L _ (T.FunDec (L _ id) args ret _)) = insertVar id . Fun $ #domains @= map (TName . typeid) args <: #codomain @= retty <: nil
--   where
--     typeid (L _ (T.Field _ _ id)) = id
--     retty = case ret of
--       Just retid -> TName retid
--       Nothing -> TUnit

-- typingDec :: HasTypingEff xs f => T.LDec -> Eff xs (Maybe Type)
-- typingDec (L loc (T.FunDec _ args ret body)) = do
--   argsty <- mapM typingField args
--   retty <- maybe (pure TUnit) lookupTypeIdEff ret
--   withVEnvScope $ do
--     mapM_ (\(id, ty) -> insertVar id (Var ty)) argsty
--     bodyty <- typingExp body
--     if bodyty == retty
--       then pure Nothing
--       else throwEff #translateError . L loc $ ExpectedType body retty bodyty
-- typingDec (L loc (T.VarDec (L _ id) _ (Just typeid) e)) = do
--   -- ty <- lookupTypeIdEff typeid >>= skipName
--   ty <- lookupSkipName typeid
--   ty' <- typingExp e
--   if ty <= ty' -- opposite to subtyping
--     then modifyEff #varEnv (E.insert id (Var ty)) >> pure Nothing
--     else throwEff #translateError . L loc $ VariableMismatchedWithDeclaredType id ty ty'
-- typingDec (L loc (T.VarDec (L _ id) _ Nothing e)) = do
--   t <- typingExp e
--   when (t == TNil) . throwEff #translateError . L loc $ AssignNilWithNoRecordAnnotation
--   modifyEff #varEnv $ E.insert id (Var t)
--   pure Nothing
-- typingDec (L _ (T.TypeDec _ ty)) = Just <$> typingType ty

-- typingType :: (
--     Lookup xs "typeEnv" (State TEnv)
--   , Lookup xs "translateError" (EitherEff (RealLocated TranslateError))
--   , Lookup xs "id" (State Unique)
--   ) => T.LType -> Eff xs Type
-- typingType (L _ (T.TypeId typeid)) = lookupTypeIdEff typeid
-- typingType (L _ (T.RecordType fields)) = do
--   fieldmap <- foldM (\e field -> (\(id, ty) -> Map.insert id ty e) <$> typingField field) Map.empty fields
--   id <- getUniqueEff #id
--   pure . TRecord $ #map @= fieldmap <: #id @= id <: nil
-- typingType (L _ (T.ArrayType typeid)) = do
--   ty <- lookupTypeIdEff typeid
--   id <- getUniqueEff #id
--   pure . TArray $ #range @= ty <: #id @= id <: nil

-- typingField :: (
--     Lookup xs "typeEnv" (State TEnv)
--   , Lookup xs "translateError" (EitherEff (RealLocated TranslateError))
--   ) => T.LField -> Eff xs (Id, Type)
-- typingField (L _ (T.Field (L _ id) _ typeid)) = (id,) <$> lookupTypeIdEff typeid


