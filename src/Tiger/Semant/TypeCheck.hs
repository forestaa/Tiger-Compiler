module Tiger.Semant.TypeCheck where

import Control.Monad.Except
import Coroutine
import Data.Extensible
import Data.Extensible.Effect
import Data.Extensible.Effect.Default
import Data.Foldable
import Data.Graph
import Env qualified as E
import Id
import RIO
import RIO.List qualified as List
import RIO.Set qualified as Set
import SrcLoc
import Tiger.LSyntax qualified as T
import Tiger.Semant.Env
import Tiger.Semant.Types
import Unique

initTEnv :: TEnv
initTEnv = E.fromList [("string", TString), ("int", TInt)]

insertInitVTEnv :: forall xs. (Lookup xs "varTypeEnv" (State VTEnv)) => Eff xs ()
insertInitVTEnv = mapM_ insertFunType initVEnv
  where
    insertFunType :: (Id, [Type], Type) -> Eff xs ()
    insertFunType (name, domains, codomain) =
      insertVarType name . FunType $ #domains @= domains <: #codomain @= codomain <: nil
    initVEnv =
      [ ("print", [TString], TUnit),
        ("flush", [], TUnit),
        ("getchar", [], TString),
        ("ord", [TString], TInt),
        ("chr", [TInt], TString),
        ("size", [TString], TInt),
        ("substring", [TString, TInt, TInt], TString),
        ("concat", [TString, TString], TString),
        ("not", [TInt], TInt),
        ("exit", [TInt], TUnit)
      ]

data TypeCheckError
  = VariableUndefined Id
  | VariableMismatchedWithDeclaredType Id Type Type
  | UnknownType Id
  | ExpectedType T.LExp Type Type
  | ExpectedTypes [T.LExp] [Type] [Type]
  | ExpectedUnitType T.LExp Type
  | ExpectedIntType T.LExp Type
  | ExpectedRecordType T.LValue Type
  | ExpectedArrayType T.LValue Type
  | ExpectedVariable Id
  | ExpectedExpression T.LExp
  | ExpectedFunction Id
  | ExpectedTypeForRecordField T.LExp Id Type Type
  | MissingRecordField T.LValue Type Id
  | MissingRecordFieldInConstruction T.LExp Type Id
  | ExtraRecordFieldInConstruction T.LExp Type Id
  | InvalidRecTypeDeclaration [RealLocated TypeDec]
  | MultiDeclaredName [LId]
  | NotDeterminedNilType
  | NotImplemented String

instance Show TypeCheckError where
  show (VariableUndefined id) = "undefined variable: " ++ show id
  show (VariableMismatchedWithDeclaredType id ty ty') = concat ["Couldn't match type: expression doesn't match with declared type: id = ", show id, ", declared type: ", show ty, ", actual type: ", show ty']
  show (UnknownType id) = "undefined type: " ++ show id
  show (ExpectedType (L _ e) ty ty') = concat ["Couldn't mach type: ", show ty, " type expected: exp = ", show e, ", actual type = ", show ty']
  show (ExpectedTypes es types types') =
    concat
      [ "Couldn't match types: ",
        show types,
        " exptected: exps = ",
        show es,
        ", actual types = ",
        show types'
      ]
  show (ExpectedUnitType (L _ e) ty) = concat ["Couldn't match type: unit type expected: exp = ", show e, ", actual type: ", show ty]
  show (ExpectedIntType (L _ e) ty) = concat ["Couldn't match type: int type expected: exp = ", show e, ", actual type: ", show ty]
  show (ExpectedRecordType (L _ v) ty) = concat ["Couldn't match type: record type expected: value = ", show v, ", actual type: ", show ty]
  show (ExpectedArrayType (L _ v) ty) = concat ["Couldn't match type: array type expected: value = ", show v, ", actual type: ", show ty]
  show (ExpectedVariable id) = concat ["Expected Variable: value = ", show id]
  show (ExpectedExpression (L _ e)) = concat ["Expected Expression: ", show e]
  show (ExpectedFunction id) = concat ["Expected Function: id = ", show id]
  show (ExpectedTypeForRecordField (L _ e) id ty ty') = concat ["Couldn't match type: ", show ty, " type expected at field ", show id, ": exp = ", show e, ", actual type: ", show ty']
  show (MissingRecordField (L _ v) ty id) = concat ["Missing record field: value = ", show v, ", type = ", show ty, ", field = ", show id]
  show (MissingRecordFieldInConstruction (L _ v) ty id) = concat ["Missing record field in construction: value = ", show v, ", type = ", show ty, ", field = ", show id]
  show (ExtraRecordFieldInConstruction (L _ v) ty id) = concat ["Record does not have field ", show id, ": value = ", show v, ", type = ", show ty]
  show (InvalidRecTypeDeclaration decs) = concat ["Found circle type declarations: decs = ", show decs]
  show (MultiDeclaredName decs) = concat ["Same name types, vars or functions declared: decs = ", show decs]
  show NotDeterminedNilType = concat ["Couldn't determine the type of nil"]
  show (NotImplemented msg) = "not implemented: " ++ msg

newtype FunDec = FunDec (Record '["id" >: LId, "args" >: [T.LField], "rettype" >: Maybe LId, "body" >: T.LExp])

newtype VarDec = VarDec (Record '["id" >: LId, "escape" >: Bool, "type" >: Maybe LId, "init" >: T.LExp])

newtype TypeDec = TypeDec (Record '["id" >: LId, "type" >: T.LType]) deriving (Show)

data Decs = FunDecs [RealLocated FunDec] | VarDecs [RealLocated VarDec] | TypeDecs [RealLocated TypeDec]

lookupTypeIdEff ::
  ( Lookup xs "typeEnv" (State TEnv),
    Lookup xs "typeCheckError" (EitherEff (RealLocated TypeCheckError))
  ) =>
  LId ->
  Eff xs Type
lookupTypeIdEff (L loc id) =
  lookupTypeId id >>= \case
    Just ty -> pure ty
    Nothing -> throwEff #typeCheckError . L loc $ UnknownType id

lookupVarTypeEff ::
  ( Lookup xs "varTypeEnv" (State VTEnv),
    Lookup xs "typeCheckError" (EitherEff (RealLocated TypeCheckError))
  ) =>
  LId ->
  Eff xs VarType
lookupVarTypeEff (L loc id) =
  lookupVarType id >>= \case
    Just ty -> pure ty
    Nothing -> throwEff #typeCheckError . L loc $ VariableUndefined id

skipName ::
  ( Lookup xs "typeEnv" (State TEnv),
    Lookup xs "typeCheckError" (EitherEff (RealLocated TypeCheckError))
  ) =>
  Type ->
  Eff xs Type
skipName (TName lid) = lookupTypeIdEff lid >>= skipName
skipName a@(TArray arr) = case arr ^. #range of
  TName id -> do
    ty <- skipName (TName id)
    pure . TArray $ set #range ty arr
  _ -> pure a
skipName ty = pure ty

lookupSkipName ::
  ( Lookup xs "typeEnv" (State TEnv),
    Lookup xs "typeCheckError" (EitherEff (RealLocated TypeCheckError))
  ) =>
  LId ->
  Eff xs Type
lookupSkipName = skipName <=< lookupTypeIdEff

checkInt :: (Lookup xs "typeCheckError" (EitherEff (RealLocated TypeCheckError))) => Type -> T.LExp -> Eff xs ()
checkInt ty e@(L loc _) =
  unless (ty == TInt) . throwEff #typeCheckError . L loc $ ExpectedIntType e ty

checkUnit :: (Lookup xs "typeCheckError" (EitherEff (RealLocated TypeCheckError))) => Type -> T.LExp -> Eff xs ()
checkUnit ty e@(L loc _) =
  unless (ty == TUnit) . throwEff #typeCheckError . L loc $ ExpectedUnitType e ty

typeCheckInt :: Type
typeCheckInt = TInt

typeCheckString :: Type
typeCheckString = TString

typeCheckNil :: Type
typeCheckNil = TNil

typeCheckId ::
  ( Lookup xs "varTypeEnv" (State VTEnv),
    Lookup xs "typeCheckError" (EitherEff (RealLocated TypeCheckError))
  ) =>
  LId ->
  Coroutine '[] (Eff xs) Type
typeCheckId lid@(L loc id) =
  lookupVarTypeEff lid >>= \case
    VarType ty -> pure ty
    FunType _ -> throwEff #typeCheckError . L loc $ ExpectedVariable id

typeCheckRecField ::
  ( Lookup xs "typeEnv" (State TEnv),
    Lookup xs "typeCheckError" (EitherEff (RealLocated TypeCheckError))
  ) =>
  RealLocated (T.LValue, Id) ->
  Coroutine '[(T.LValue, Type)] (Eff xs) Type
typeCheckRecField (L loc (lv, field)) =
  yield @'[] lv $
    skipName >=> \case
      valueTy@(TRecord r) -> case List.lookup field (r ^. #map) of
        Just ty -> pure ty
        Nothing -> throwEff #typeCheckError . L loc $ MissingRecordField lv valueTy field
      valueTy -> throwEff #typeCheckError . L loc $ ExpectedRecordType lv valueTy

typeCheckArrayIndex ::
  ( Lookup xs "typeEnv" (State TEnv),
    Lookup xs "typeCheckError" (EitherEff (RealLocated TypeCheckError))
  ) =>
  RealLocated (T.LValue, T.LExp) ->
  Coroutine '[(T.LValue, Type), (T.LExp, Type)] (Eff xs) Type
typeCheckArrayIndex (L loc (lv, le)) =
  yield @'[(T.LExp, Type)] lv $
    skipName >=> \case
      TArray a -> yield @'[] le $ \indexTy -> do
        checkInt indexTy le
        pure $ a ^. #range
      ty -> throwEff #typeCheckError . L loc $ ExpectedArrayType lv ty

typeCheckBinOp :: (Lookup xs "typeCheckError" (EitherEff (RealLocated TypeCheckError))) => RealLocated (T.LOp', T.LExp, T.LExp) -> Coroutine '[(T.LExp, Type), (T.LExp, Type)] (Eff xs) Type
typeCheckBinOp (L loc (op, left, right)) =
  yield @'[(T.LExp, Type)] left $ \leftTy ->
    yield @'[] right $ \rightTy ->
      if
          | not (isEqNEq op) -> checkInt leftTy left >> checkInt rightTy right >> pure TInt
          | isUnit leftTy -> throwEff #typeCheckError . L loc $ ExpectedExpression left
          | isUnit rightTy -> throwEff #typeCheckError . L loc $ ExpectedExpression right
          | leftTy == TNil && rightTy == TNil -> throwEff #typeCheckError . L loc $ NotDeterminedNilType
          | not (isComparable leftTy rightTy) -> throwEff #typeCheckError . L loc $ ExpectedType right leftTy rightTy
          | otherwise -> pure TInt
  where
    isEqNEq T.Eq = True
    isEqNEq T.NEq = True
    isEqNEq _ = False

    isUnit TUnit = True
    isUnit _ = False

typeCheckIfElse :: (Lookup xs "typeCheckError" (EitherEff (RealLocated TypeCheckError))) => RealLocated (T.LExp, T.LExp, T.LExp) -> Coroutine '[(T.LExp, Type), (T.LExp, Type), (T.LExp, Type)] (Eff xs) Type
typeCheckIfElse (L loc (bool, then', else')) =
  yield @'[(T.LExp, Type), (T.LExp, Type)] bool $ \boolTy -> do
    checkInt boolTy bool
    yield @'[(T.LExp, Type)] then' $ \thenTy ->
      yield @'[] else' $ \elseTy ->
        if isComparable thenTy elseTy
          then pure thenTy
          else throwEff #typeCheckError . L loc $ ExpectedType else' thenTy elseTy

typeCheckIfNoElse :: (Lookup xs "typeCheckError" (EitherEff (RealLocated TypeCheckError))) => (T.LExp, T.LExp) -> Coroutine '[(T.LExp, Type), (T.LExp, Type)] (Eff xs) Type
typeCheckIfNoElse (bool, then') =
  yield @'[(T.LExp, Type)] bool $ \boolTy -> do
    checkInt boolTy bool
    yield @'[] then' $ \thenTy -> do
      checkUnit thenTy then'
      pure TUnit

typeCheckRecordCreation ::
  forall xs.
  ( Lookup xs "typeEnv" (State TEnv),
    Lookup xs "typeCheckError" (EitherEff (RealLocated TypeCheckError))
  ) =>
  RealLocated (LId, [T.LFieldAssign]) ->
  Coroutine '[([T.LFieldAssign], [(Id, Type)])] (Eff xs) Type
typeCheckRecordCreation (L loc (typeid, fields)) =
  lookupTypeIdEff typeid >>= \case
    ty@(TRecord r) ->
      yield @'[] fields $ \fieldsTy -> do
        typecheckFields ty (r ^. #map) fieldsTy
        pure ty
    ty -> throwEff #typeCheckError . L loc $ ExpectedRecordType (L loc (T.Id typeid)) ty
  where
    typecheckFields :: Type -> [(Id, Type)] -> [(Id, Type)] -> Eff xs ()
    typecheckFields _ [] [] = pure ()
    typecheckFields ty ((id1, _) : _) [] = throwEff #typeCheckError . L loc $ MissingRecordFieldInConstruction (L loc $ T.RecordCreate typeid fields) ty id1
    typecheckFields ty [] ((id2, _) : _) = throwEff #typeCheckError . L loc $ ExtraRecordFieldInConstruction (L loc $ T.RecordCreate typeid fields) ty id2
    typecheckFields ty ((id1, TName lid) : as) bs = do
      ty1 <- lookupSkipName lid
      typecheckFields ty ((id1, ty1) : as) bs
    typecheckFields ty as ((id2, TName lid) : bs) = do
      ty2 <- lookupSkipName lid
      typecheckFields ty as ((id2, ty2) : bs)
    typecheckFields ty ((id1, ty1) : as) ((id2, ty2) : bs)
      | id1 < id2 = throwEff #typeCheckError . L loc $ MissingRecordFieldInConstruction (L loc $ T.RecordCreate typeid fields) ty id1
      | id1 > id2 = throwEff #typeCheckError . L loc $ ExtraRecordFieldInConstruction (L loc $ T.RecordCreate typeid fields) ty id2
      | ty1 > ty2 = throwEff #typeCheckError . L loc $ ExpectedTypeForRecordField (L loc $ T.RecordCreate typeid fields) id1 ty1 ty2
      | otherwise = typecheckFields ty as bs

typeCheckArrayCreation ::
  ( Lookup xs "typeEnv" (State TEnv),
    Lookup xs "typeCheckError" (EitherEff (RealLocated TypeCheckError))
  ) =>
  RealLocated (LId, T.LExp, T.LExp) ->
  Coroutine '[(T.LExp, Type), (T.LExp, Type)] (Eff xs) Type
typeCheckArrayCreation (L loc (typeid, size, init)) =
  lookupSkipName typeid >>= \case
    ty@(TArray a) ->
      yield @'[(T.LExp, Type)] size $ \sizeTy -> do
        checkInt sizeTy size
        yield @'[] init $ \initTy ->
          if a ^. #range <= initTy
            then pure ty
            else throwEff #typeCheckError . L loc $ ExpectedType init (a ^. #range) initTy
    ty -> throwEff #typeCheckError . L loc $ ExpectedArrayType (L loc (T.Id typeid)) ty

typeCheckWhileLoop ::
  ( Lookup xs "typeCheckError" (EitherEff (RealLocated TypeCheckError))
  ) =>
  (T.LExp, T.LExp) ->
  Coroutine '[(T.LExp, Type), (T.LExp, Type)] (Eff xs) Type
typeCheckWhileLoop (bool, body) =
  yield @'[(T.LExp, Type)] bool $ \boolTy -> do
    checkInt boolTy bool
    yield @'[] body $ \bodyTy -> do
      checkUnit bodyTy body
      pure TUnit

typeCheckForLoop ::
  ( Lookup xs "varTypeEnv" (State VTEnv),
    Lookup xs "typeCheckError" (EitherEff (RealLocated TypeCheckError))
  ) =>
  (Id, T.LExp, T.LExp, T.LExp) ->
  Coroutine '[(T.LExp, Type), (T.LExp, Type), (T.LExp, Type)] (Eff xs) Type
typeCheckForLoop (index, from, to, body) = do
  insertVarType index $ VarType TInt
  yield @'[(T.LExp, Type), (T.LExp, Type)] from $ \fromTy -> do
    checkInt fromTy from
    yield @'[(T.LExp, Type)] to $ \toTy -> do
      checkInt toTy to
      yield @'[] body $ \bodyTy -> do
        checkUnit bodyTy body
        pure TUnit

typeCheckBreak :: Type
typeCheckBreak = TUnit

typeCheckFunApply ::
  ( Lookup xs "varTypeEnv" (State VTEnv),
    Lookup xs "typeEnv" (State TEnv),
    Lookup xs "typeCheckError" (EitherEff (RealLocated TypeCheckError))
  ) =>
  RealLocated (LId, [T.LExp]) ->
  Coroutine '[([T.LExp], [Type])] (Eff xs) Type
typeCheckFunApply (L loc (func, args)) =
  lookupVarTypeEff func >>= \case
    FunType r ->
      yield @'[] args $ \argsTy -> do
        domains <- mapM skipName $ r ^. #domains
        if length domains == length argsTy && domains <= argsTy
          then pure $ r ^. #codomain
          else throwEff #typeCheckError . L loc $ ExpectedTypes args domains argsTy
    VarType _ -> throwEff #typeCheckError . L loc $ ExpectedFunction (unLId func)

typeCheckAssign ::
  ( Lookup xs "typeCheckError" (EitherEff (RealLocated TypeCheckError))
  ) =>
  (T.LValue, T.LExp) ->
  Coroutine '[(T.LValue, Type), (T.LExp, Type)] (Eff xs) Type
typeCheckAssign (lv, le@(L loc _)) =
  yield @'[(T.LExp, Type)] lv $ \valueTy ->
    yield @'[] le $ \expTy ->
      if valueTy <= expTy
        then pure TUnit
        else throwEff #typeCheckError . L loc $ ExpectedType le valueTy expTy

typeCheckSeq :: [T.LExp] -> Coroutine '[([T.LExp], [Type])] (Eff xs) Type
typeCheckSeq es =
  yield @'[] es $ \expTys ->
    case List.lastMaybe expTys of
      Just ty -> pure ty
      Nothing -> pure TUnit

typeCheckLet ::
  ( Lookup xs "typeEnv" (State TEnv),
    Lookup xs "varTypeEnv" (State VTEnv)
  ) =>
  ([T.LDec], T.LExp) ->
  Coroutine '[([Decs], ()), (T.LExp, Type)] (Eff xs) Type
typeCheckLet (decs, body) = do
  beginTEnvScope
  beginVTEnvScope
  yield @'[(T.LExp, Type)] (groupByDecType decs) $ \() ->
    yield @'[] body $ \bodyTy -> do
      endVTEnvScope
      endTEnvScope
      pure bodyTy

groupByDecType :: [T.LDec] -> [Decs]
groupByDecType = foldr go []
  where
    convertFunDec (L loc (T.FunDec id args rettype body)) = L loc . FunDec $ #id @= id <: #args @= args <: #rettype @= rettype <: #body @= body <: nil
    convertFunDec _ = undefined
    convertVarDec (L loc (T.VarDec id escape ty init)) = L loc . VarDec $ #id @= id <: #escape @= escape <: #type @= ty <: #init @= init <: nil
    convertVarDec _ = undefined
    convertTypeDec (L loc (T.TypeDec id ty)) = L loc . TypeDec $ #id @= id <: #type @= ty <: nil
    convertTypeDec _ = undefined

    go d@(L _ T.FunDec {}) [] = [FunDecs [convertFunDec d]]
    go d@(L _ T.FunDec {}) (FunDecs ds : acc) = FunDecs (convertFunDec d : ds) : acc
    go d@(L _ T.FunDec {}) acc = FunDecs [convertFunDec d] : acc
    go d@(L _ T.VarDec {}) [] = [VarDecs [convertVarDec d]]
    go d@(L _ T.VarDec {}) (VarDecs ds : acc) = VarDecs (convertVarDec d : ds) : acc
    go d@(L _ T.VarDec {}) acc = VarDecs [convertVarDec d] : acc
    go d@(L _ T.TypeDec {}) [] = [TypeDecs [convertTypeDec d]]
    go d@(L _ T.TypeDec {}) (TypeDecs ds : acc) = TypeDecs (convertTypeDec d : ds) : acc
    go d@(L _ T.TypeDec {}) acc = TypeDecs [convertTypeDec d] : acc

typeCheckVarDec ::
  ( Lookup xs "varTypeEnv" (State VTEnv),
    Lookup xs "typeEnv" (State TEnv),
    Lookup xs "typeCheckError" (EitherEff (RealLocated TypeCheckError))
  ) =>
  RealLocated VarDec ->
  Coroutine '[(T.LExp, Type)] (Eff xs) ()
typeCheckVarDec (L loc (VarDec r)) =
  yield @'[] (r ^. #init) $ \initTy ->
    case r ^. #type of
      Nothing | initTy == TNil -> throwEff #typeCheckError . L loc $ NotDeterminedNilType
      Nothing -> insertVarType (unLId $ r ^. #id) $ VarType initTy
      Just typeid -> do
        declaredTy <- lookupSkipName typeid
        if declaredTy <= initTy -- opposite to subtyping
          then insertVarType (unLId $ r ^. #id) $ VarType declaredTy
          else throwEff #typeCheckError . L loc $ ExpectedType (r ^. #init) declaredTy initTy

typeCheckFunDecs ::
  forall xs.
  ( Lookup xs "varTypeEnv" (State VTEnv),
    Lookup xs "typeEnv" (State TEnv),
    Lookup xs "typeCheckError" (EitherEff (RealLocated TypeCheckError))
  ) =>
  [RealLocated FunDec] ->
  Eff xs ()
typeCheckFunDecs ds = do
  checkSameNameDec $ fmap extractLId ds
  mapM_ insertFunType ds
  where
    extractLId (L _ (FunDec r)) = r ^. #id
    insertFunType :: RealLocated FunDec -> Eff xs ()
    insertFunType (L _ (FunDec dec)) = do
      domains <- mapM (\(L _ (T.Field _ _ typeid)) -> lookupTypeIdEff typeid) $ dec ^. #args
      codomain <- maybe (pure TUnit) lookupTypeIdEff $ dec ^. #rettype
      insertVarType (unLId $ dec ^. #id) . FunType $ #domains @= domains <: #codomain @= codomain <: nil

typeCheckFunDec ::
  forall xs.
  ( Lookup xs "varTypeEnv" (State VTEnv),
    Lookup xs "typeEnv" (State TEnv),
    Lookup xs "typeCheckError" (EitherEff (RealLocated TypeCheckError))
  ) =>
  RealLocated FunDec ->
  Coroutine '[(T.LExp, Type)] (Eff xs) ()
typeCheckFunDec (L loc (FunDec dec)) = do
  beginVTEnvScope
  mapM_ insertParameterVarType $ dec ^. #args
  yield @'[] (dec ^. #body) $ \bodyTy -> do
    declaredTy <- maybe (pure TUnit) lookupSkipName $ dec ^. #rettype
    if declaredTy <= bodyTy
      then endVTEnvScope
      else throwEff #typeCheckError . L loc $ ExpectedType (dec ^. #body) declaredTy bodyTy
  where
    insertParameterVarType :: T.LField -> Eff xs ()
    insertParameterVarType (L _ (T.Field (L _ id) _ typeid)) = lookupTypeIdEff typeid >>= insertVarType id . VarType

typeCheckTypeDecs ::
  ( Lookup xs "typeEnv" (State TEnv),
    Lookup xs "typeCheckError" (EitherEff (RealLocated TypeCheckError)),
    Lookup xs "id" UniqueEff
  ) =>
  [RealLocated TypeDec] ->
  Eff xs ()
typeCheckTypeDecs ds = do
  let typeLIds = fmap extractLId ds
  checkSameNameDec typeLIds
  checkInvalidRecType ds
  types <- withTEnvScope $ do
    mapM_ (\lid -> insertType (unLId lid) (TName lid)) typeLIds
    mapM (\(L _ (TypeDec r)) -> (unLId (r ^. #id),) <$> typeCheckType (r ^. #type)) ds
  mapM_ (uncurry insertType) types
  where
    extractLId (L _ (TypeDec r)) = r ^. #id

checkSameNameDec :: Lookup xs "typeCheckError" (EitherEff (RealLocated TypeCheckError)) => [LId] -> Eff xs ()
checkSameNameDec ids = case runCheckSameNameDec ids of
  Right _ -> pure ()
  Left loc -> throwEff #typeCheckError . L loc $ MultiDeclaredName ids
  where
    runCheckSameNameDec = leaveEff . runEitherDef . flip runReaderDef Set.empty . checkSameNameDec'

    checkSameNameDec' [] = pure ()
    checkSameNameDec' (L loc id : ids) = flip (runContEff @"cont") pure $ do
      asks (Set.member id) >>= bool (pure ()) (contEff #cont $ const (throwError loc))
      local (Set.insert id) . castEff $ checkSameNameDec' ids

checkInvalidRecType :: (Lookup xs "typeCheckError" (EitherEff (RealLocated TypeCheckError))) => [RealLocated TypeDec] -> Eff xs ()
checkInvalidRecType decs =
  if any isCycle $ stronglyConnComp graph
    then throwEff #typeCheckError . L undefined $ InvalidRecTypeDeclaration decs
    else pure ()
  where
    typeDecToNode (L _ (TypeDec r)) = (r ^. #id, unLId $ r ^. #id, typeToEdge (r ^. #type))
      where
        typeToEdge (L _ (T.TypeId (L _ id'))) = [id']
        typeToEdge _ = []
    graph = typeDecToNode <$> decs
    isCycle (CyclicSCC _) = True
    isCycle _ = False

typeCheckType ::
  ( Lookup xs "typeEnv" (State TEnv),
    Lookup xs "typeCheckError" (EitherEff (RealLocated TypeCheckError)),
    Lookup xs "id" UniqueEff
  ) =>
  T.LType ->
  Eff xs Type
typeCheckType (L _ (T.TypeId typeid)) = lookupTypeIdEff typeid
typeCheckType (L _ (T.RecordType fields)) = do
  fieldmap <- foldrM (\field e -> (\(id, ty) -> (:) (id, ty) e) <$> typeCheckField field) [] fields
  id <- getUniqueEff #id
  pure . TRecord $ #map @= fieldmap <: #id @= id <: nil
typeCheckType (L _ (T.ArrayType typeid)) = do
  ty <- lookupTypeIdEff typeid
  id <- getUniqueEff #id
  pure . TArray $ #range @= ty <: #id @= id <: nil

typeCheckField ::
  ( Lookup xs "typeEnv" (State TEnv),
    Lookup xs "typeCheckError" (EitherEff (RealLocated TypeCheckError))
  ) =>
  T.LField ->
  Eff xs (Id, Type)
typeCheckField (L _ (T.Field (L _ id) _ typeid)) = (id,) <$> lookupTypeIdEff typeid
