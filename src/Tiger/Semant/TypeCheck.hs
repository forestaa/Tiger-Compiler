module Tiger.Semant.TypeCheck where

import           Control.Monad.Except
import           Data.Extensible
import           Data.Extensible.Effect.Default
import           Data.Foldable
import           Data.Graph
import           RIO
import qualified RIO.List as List
import qualified RIO.Set as Set

import           Coroutine
import           SrcLoc
import           Unique
import           Id

import           Tiger.Semant.Env
import           Tiger.Semant.Types
import qualified Tiger.LSyntax as T


data TranslateError =
  -- typing
    VariableUndefined Id
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

  -- translate
  | BreakOutsideLoop

  | NotImplemented String
instance Show TranslateError where
  show (VariableUndefined id) = "undefined variable: " ++ show id
  show (VariableMismatchedWithDeclaredType id ty ty') = concat ["Couldn't match type: expression doesn't match with declared type: id = ", show id, ", declared type: ", show ty, ", actual type: ", show ty']
  show (UnknownType id) = "undefined type: " ++ show id
  show (ExpectedType (L _ e) ty ty') = concat ["Couldn't mach type: ", show ty, " type expected: exp = ", show e, ", actual type = ", show ty']
  show (ExpectedTypes es types types') = concat [
    "Couldn't match types: ", show types, " exptected: exps = ", show es, ", actual types = ", show types']
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

  show BreakOutsideLoop = "break should be inside while or for loop"
  show (NotImplemented msg) = "not implemented: " ++ msg

newtype FunDec = FunDec (Record '["id" >: LId, "args" >: [T.LField], "rettype" >: Maybe LId, "body" >: T.LExp])
newtype VarDec = VarDec (Record '["id" >: LId, "escape" >: Bool, "type" >: Maybe LId, "init" >: T.LExp])
newtype TypeDec = TypeDec (Record '["id" >: LId, "type" >: T.LType]) deriving Show
data Decs = FunDecs [RealLocated FunDec] | VarDecs [RealLocated VarDec] | TypeDecs [RealLocated TypeDec]

lookupTypeIdEff :: (Lookup xs "typeEnv" (State TEnv), Lookup xs "translateError" (EitherEff (RealLocated TranslateError))) => LId -> Eff xs Type
lookupTypeIdEff (L loc id) = lookupTypeId id >>= maybe (throwEff #translateError . L loc $ UnknownType id) pure
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
    ty <- skipName (TName id)
    pure . TArray $ set #range ty arr
  _ -> pure a
skipName ty = pure ty
lookupSkipName :: (
    Lookup xs "typeEnv" (State TEnv)
  , Lookup xs "translateError" (EitherEff (RealLocated TranslateError))
  ) => LId -> Eff xs Type
lookupSkipName = skipName <=< lookupTypeIdEff

checkInt :: (Lookup xs "translateError" (EitherEff (RealLocated TranslateError))) => Type -> T.LExp -> Eff xs ()
checkInt ty e@(L loc _) =
  unless (ty == TInt) . throwEff #translateError . L loc $ ExpectedIntType e ty
checkUnit :: (Lookup xs "translateError" (EitherEff (RealLocated TranslateError))) => Type -> T.LExp -> Eff xs ()
checkUnit ty e@(L loc _) =
    unless (ty == TUnit) . throwEff #translateError . L loc $ ExpectedUnitType e ty

typeCheckInt :: Type
typeCheckInt = TInt
typeCheckString :: Type
typeCheckString = TString
typeCheckNil :: Type
typeCheckNil = TNil

typeCheckId :: (
    Lookup xs "varEnv" (State (VEnv f))
  , Lookup xs "translateError" (EitherEff (RealLocated TranslateError))
  ) => LId -> Coroutine '[] (Eff xs) Type
typeCheckId lid@(L loc id) = lookupVarIdEff lid >>= \case
  Var r -> pure $ r ^. #type
  Fun _ -> throwEff #translateError . L loc $ ExpectedVariable id

typeCheckRecField :: (
    Lookup xs "typeEnv" (State TEnv)
  , Lookup xs "translateError" (EitherEff (RealLocated TranslateError))
  ) => RealLocated (T.LValue, Id) -> Coroutine '[(T.LValue, Type)] (Eff xs) Type
typeCheckRecField (L loc (lv, field)) = yield @'[] lv $
  skipName >=> \case
    valueTy@(TRecord r) -> case List.lookup field (r ^. #map) of
      Just ty -> pure ty
      Nothing -> throwEff #translateError . L loc $ MissingRecordField lv valueTy field
    valueTy -> throwEff #translateError . L loc $ ExpectedRecordType lv valueTy

typeCheckArrayIndex :: (
    Lookup xs "typeEnv" (State TEnv)
  , Lookup xs "translateError" (EitherEff (RealLocated TranslateError))
  ) => RealLocated (T.LValue, T.LExp) -> Coroutine '[(T.LValue, Type), (T.LExp, Type)] (Eff xs) Type
typeCheckArrayIndex (L loc (lv, le)) = yield @'[(T.LExp, Type)] lv $
  skipName >=> \case
    TArray a -> yield @'[] le $ \indexTy -> do
      checkInt indexTy le
      pure $ a ^. #range
    ty -> throwEff #translateError . L loc $ ExpectedArrayType lv ty

typeCheckBinOp :: (Lookup xs "translateError" (EitherEff (RealLocated TranslateError))) => RealLocated (T.LOp', T.LExp, T.LExp) -> Coroutine '[(T.LExp, Type), (T.LExp, Type)] (Eff xs) Type
typeCheckBinOp (L loc (op, left, right)) =
  yield @'[(T.LExp, Type)] left $ \leftTy ->
    yield @'[] right $ \rightTy -> if
      | not (isEqNEq op) -> checkInt leftTy left >> checkInt rightTy right >> pure TInt
      | isUnit leftTy -> throwEff #translateError . L loc $ ExpectedExpression left
      | isUnit rightTy -> throwEff #translateError . L loc $ ExpectedExpression right
      | leftTy == TNil && rightTy == TNil -> throwEff #translateError . L loc $ NotDeterminedNilType
      | not (isComparable leftTy rightTy) -> throwEff #translateError . L loc $ ExpectedType right leftTy rightTy
      | otherwise -> pure TInt
  where
    isEqNEq T.Eq = True
    isEqNEq T.NEq = True
    isEqNEq _ = False

    isUnit TUnit = True
    isUnit _ = False

typeCheckIfElse :: (Lookup xs "translateError" (EitherEff (RealLocated TranslateError))) => RealLocated (T.LExp, T.LExp, T.LExp) -> Coroutine '[(T.LExp, Type), (T.LExp, Type), (T.LExp, Type)] (Eff xs) Type
typeCheckIfElse (L loc (bool, then', else')) =
  yield @'[(T.LExp, Type), (T.LExp, Type)] bool $ \boolTy -> do
    checkInt boolTy bool
    yield @'[(T.LExp, Type)] then' $ \thenTy ->
      yield @'[] else' $ \elseTy ->
        if isComparable thenTy elseTy
          then pure thenTy
          else throwEff #translateError . L loc $ ExpectedType else' thenTy elseTy

typeCheckIfNoElse :: (Lookup xs "translateError" (EitherEff (RealLocated TranslateError))) => (T.LExp, T.LExp) -> Coroutine '[(T.LExp, Type), (T.LExp, Type)] (Eff xs) Type
typeCheckIfNoElse (bool, then') =
  yield @'[(T.LExp, Type)] bool $ \boolTy -> do
    checkInt boolTy bool
    yield @'[] then' $ \thenTy -> do
      checkUnit thenTy then'
      pure TUnit

typeCheckRecordCreation :: forall xs. (
    Lookup xs "typeEnv" (State TEnv)
  , Lookup xs "translateError" (EitherEff (RealLocated TranslateError))
  ) => RealLocated (LId, [T.LFieldAssign]) -> Coroutine '[([T.LFieldAssign], [(Id, Type)])] (Eff xs) Type
typeCheckRecordCreation (L loc (typeid, fields)) =
 lookupTypeIdEff typeid >>= \case
    ty@(TRecord r) ->
      yield @'[] fields $ \fieldsTy -> do
        typecheckFields ty (r ^. #map) fieldsTy
        pure ty
    ty -> throwEff #translateError . L loc $ ExpectedRecordType (L loc (T.Id typeid)) ty
  where
    typecheckFields :: Type -> [(Id, Type)] -> [(Id, Type)] -> Eff xs ()
    typecheckFields _ [] [] = pure ()
    typecheckFields ty ((id1, _):_) [] = throwEff #translateError . L loc $ MissingRecordFieldInConstruction (L loc $ T.RecordCreate typeid fields) ty id1
    typecheckFields ty [] ((id2, _):_) = throwEff #translateError . L loc $ ExtraRecordFieldInConstruction (L loc $ T.RecordCreate typeid fields) ty id2
    typecheckFields ty ((id1, TName lid):as) bs = do
      ty1 <- lookupSkipName lid
      typecheckFields ty ((id1, ty1):as) bs
    typecheckFields ty as ((id2, TName lid):bs) = do
      ty2 <- lookupSkipName lid
      typecheckFields ty as ((id2, ty2):bs)
    typecheckFields ty ((id1, ty1):as) ((id2, ty2):bs)
      | id1 < id2 = throwEff #translateError . L loc $ MissingRecordFieldInConstruction (L loc $ T.RecordCreate typeid fields) ty id1
      | id1 > id2 = throwEff #translateError . L loc $ ExtraRecordFieldInConstruction (L loc $ T.RecordCreate typeid fields) ty id2
      |  ty1 > ty2 = throwEff #translateError . L loc $ ExpectedTypeForRecordField (L loc $ T.RecordCreate typeid fields) id1 ty1 ty2
      | otherwise = typecheckFields ty as bs

typeCheckArrayCreation :: (
      Lookup xs "typeEnv" (State TEnv)
  , Lookup xs "translateError" (EitherEff (RealLocated TranslateError))
  ) => RealLocated (LId, T.LExp, T.LExp) -> Coroutine '[(T.LExp, Type), (T.LExp, Type)] (Eff xs) Type
typeCheckArrayCreation (L loc (typeid, size, init)) =
  lookupSkipName typeid >>= \case
    ty@(TArray a) ->
      yield @'[(T.LExp, Type)] size $ \sizeTy -> do
        checkInt sizeTy size
        yield @'[] init $ \initTy ->
          if a ^. #range <= initTy
            then pure ty
            else throwEff #translateError . L loc $ ExpectedType init (a ^. #range) initTy
    ty -> throwEff #translateError . L loc $ ExpectedArrayType (L loc (T.Id typeid)) ty

typeCheckWhileLoop :: (
    Lookup xs "translateError" (EitherEff (RealLocated TranslateError))
  ) => (T.LExp, T.LExp) -> Coroutine '[(T.LExp, Type), (T.LExp, Type)] (Eff xs) Type
typeCheckWhileLoop (bool, body) =
  yield @'[(T.LExp, Type)] bool $ \boolTy -> do
    checkInt boolTy bool
    yield @'[] body $ \bodyTy -> do
      checkUnit bodyTy body
      pure TUnit

-- TODO: separate varEnv into accessEnv and typeEnv
typeCheckForLoop :: (
    Lookup xs "translateError" (EitherEff (RealLocated TranslateError))
  ) => (T.LExp, T.LExp, T.LExp) -> Coroutine '[(T.LExp, Type), (T.LExp, Type), (T.LExp, Type)] (Eff xs) Type
typeCheckForLoop (from, to, body) =
  yield @'[(T.LExp, Type), (T.LExp, Type)] from $ \fromTy -> do
    checkInt fromTy from
    yield @'[(T.LExp, Type)] to $ \toTy -> do
      checkInt toTy to
      yield @'[] body $ \bodyTy -> do
        checkUnit bodyTy body
        pure TUnit

typeCheckBreak :: Type
typeCheckBreak = TUnit

typeCheckFunApply :: (
    Lookup xs "varEnv" (State (VEnv f))
  , Lookup xs "typeEnv" (State TEnv)
  , Lookup xs "translateError" (EitherEff (RealLocated TranslateError))
  ) => RealLocated (LId, [T.LExp]) -> Coroutine '[([T.LExp], [Type])] (Eff xs) Type
typeCheckFunApply (L loc (func, args)) =
  lookupVarIdEff func >>= \case
    Fun r ->
      yield @'[] args $ \argsTy -> do
        domains <- mapM skipName $ r ^. #domains
        if length domains == length argsTy && domains <= argsTy
          then pure $ r ^. #codomain
          else throwEff #translateError . L loc $ ExpectedTypes args domains argsTy
    Var _ -> throwEff #translateError . L loc $ ExpectedFunction (unLId func)

typeCheckAssign :: (
    Lookup xs "translateError" (EitherEff (RealLocated TranslateError))
  ) => (T.LValue, T.LExp) -> Coroutine '[(T.LValue, Type), (T.LExp, Type)] (Eff xs) Type
typeCheckAssign (lv, le@(L loc _)) =
  yield @'[(T.LExp, Type)] lv $ \valueTy ->
    yield @'[] le $ \expTy ->
      if valueTy <= expTy
        then pure TUnit
        else throwEff #translateError . L loc $ ExpectedType le valueTy expTy

typeCheckSeq :: [T.LExp] -> Coroutine '[([T.LExp], [Type])] (Eff xs) Type
typeCheckSeq es =
  yield @'[] es $ \expTys ->
    case List.lastMaybe expTys of
      Just ty -> pure ty
      Nothing -> pure TUnit

typeCheckLet :: (
    Lookup xs "typeEnv" (State TEnv)
  ) => ([T.LDec], T.LExp) -> Coroutine '[([Decs], ()), (T.LExp, Type)] (Eff xs) Type
typeCheckLet (decs, body) =
  withTEnvScope $
    yield @'[(T.LExp, Type)] (groupByDecType decs) $ \() ->
      yield @'[] body $ \bodyTy ->
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

    go d@(L _ T.FunDec{}) [] = [FunDecs [convertFunDec d]]
    go d@(L _ T.FunDec{}) (FunDecs ds : acc) = FunDecs (convertFunDec d : ds) : acc
    go d@(L _ T.FunDec{}) acc = FunDecs [convertFunDec d] : acc

    go d@(L _ T.VarDec{}) [] = [VarDecs [convertVarDec d]]
    go d@(L _ T.VarDec{}) (VarDecs ds : acc) = VarDecs (convertVarDec d:ds) : acc
    go d@(L _ T.VarDec{}) acc = VarDecs [convertVarDec d] : acc

    go d@(L _ T.TypeDec{}) [] = [TypeDecs [convertTypeDec d]]
    go d@(L _ T.TypeDec{}) (TypeDecs ds : acc) = TypeDecs (convertTypeDec d:ds) : acc
    go d@(L _ T.TypeDec{}) acc = TypeDecs [convertTypeDec d] : acc

typeCheckVarDec :: (
    Lookup xs "typeEnv" (State TEnv)
  , Lookup xs "translateError" (EitherEff (RealLocated TranslateError))
  ) => RealLocated VarDec -> Coroutine '[(T.LExp, Type)] (Eff xs) Type
typeCheckVarDec (L loc (VarDec r)) =
  yield @'[] (r ^. #init) $ \initTy ->
    case r ^. #type of
      Nothing | initTy == TNil -> throwEff #translateError . L loc $ NotDeterminedNilType
      Nothing -> pure initTy
      Just typeid -> do
        declaredTy <- lookupSkipName typeid
        if declaredTy <= initTy  -- opposite to subtyping
          then pure declaredTy
          else throwEff #translateError . L loc $ ExpectedType (r ^. #init) declaredTy initTy

typeCheckFunDecs :: Lookup xs "translateError" (EitherEff (RealLocated TranslateError)) => [RealLocated FunDec] -> Eff xs ()
typeCheckFunDecs ds = checkSameNameDec $ fmap extractLId ds
  where
    extractLId (L _ (FunDec r)) = r ^. #id

typeCheckFunDec :: (
    Lookup xs "typeEnv" (State TEnv)
  , Lookup xs "translateError" (EitherEff (RealLocated TranslateError))
  ) => RealLocated FunDec -> Coroutine '[(([(Id, Type)], T.LExp), Type)] (Eff xs) ()
typeCheckFunDec (L loc (FunDec dec)) = do
  argIdTys <- mapM (\(L _ (T.Field (L _ id) _ typeid)) -> (id, ) <$> lookupTypeIdEff typeid) $ dec ^. #args
  yield @'[] (argIdTys, dec ^. #body) $ \bodyTy -> do
    declaredTy <- maybe (pure TUnit) lookupSkipName $ dec ^. #rettype
    if declaredTy <= bodyTy
      then pure ()
      else throwEff #translateError . L loc $ ExpectedType (dec ^. #body) declaredTy bodyTy

typeCheckTypeDecs :: (
    Lookup xs "typeEnv" (State TEnv)
  , Lookup xs "translateError" (EitherEff (RealLocated TranslateError))
  , Lookup xs "id" UniqueEff
  ) => [RealLocated TypeDec] -> Eff xs ()
typeCheckTypeDecs ds = do
  let typeLIds = fmap extractLId ds
  checkSameNameDec typeLIds
  checkInvalidRecType ds
  types <- withTEnvScope $ do
    mapM_ (\lid -> insertType (unLId lid) (TName lid)) typeLIds
    mapM (\(L _ (TypeDec r)) -> (unLId (r ^. #id), ) <$> typeCheckType (r ^. #type)) ds
  mapM_ (uncurry insertType) types
  where
    extractLId (L _ (TypeDec r)) = r ^. #id

checkSameNameDec :: Lookup xs "translateError" (EitherEff (RealLocated TranslateError)) => [LId] -> Eff xs ()
checkSameNameDec ids = case runCheckSameNameDec ids of
  Right _ -> pure ()
  Left loc -> throwEff #translateError . L loc $ MultiDeclaredName ids
  where
    runCheckSameNameDec = leaveEff . runEitherDef . flip runReaderDef Set.empty . checkSameNameDec'

    checkSameNameDec' [] = pure ()
    checkSameNameDec' (L loc id:ids) = flip (runContEff @"cont") pure $ do
      asks (Set.member id) >>= bool (pure ()) (contEff #cont $ const (throwError loc))
      local (Set.insert id) . castEff $ checkSameNameDec' ids

checkInvalidRecType :: (Lookup xs "translateError" (EitherEff (RealLocated TranslateError))) => [RealLocated TypeDec] -> Eff xs ()
checkInvalidRecType decs =
  if any isCycle $ stronglyConnComp graph
    then throwEff #translateError . L undefined $ InvalidRecTypeDeclaration decs
    else pure ()
  where
    typeDecToNode (L _ (TypeDec r)) = (r ^. #id, unLId $ r ^. #id, typeToEdge (r ^. #type))
      where
        typeToEdge (L _ (T.TypeId (L _ id'))) = [id']
        typeToEdge _ = []
    graph = typeDecToNode <$> decs
    isCycle (CyclicSCC _) = True
    isCycle _ = False

typeCheckType :: (
    Lookup xs "typeEnv" (State TEnv)
  , Lookup xs "translateError" (EitherEff (RealLocated TranslateError))
  , Lookup xs "id" UniqueEff
  ) => T.LType -> Eff xs Type
typeCheckType (L _ (T.TypeId typeid)) = lookupTypeIdEff typeid
typeCheckType (L _ (T.RecordType fields)) = do
  fieldmap <- foldrM (\field e -> (\(id, ty) -> (:) (id, ty) e) <$> typeCheckField field) [] fields
  id <- getUniqueEff #id
  pure . TRecord $ #map @= fieldmap <: #id @= id <: nil
typeCheckType (L _ (T.ArrayType typeid)) = do
  ty <- lookupTypeIdEff typeid
  id <- getUniqueEff #id
  pure . TArray $ #range @= ty <: #id @= id <: nil

typeCheckField :: (
    Lookup xs "typeEnv" (State TEnv)
  , Lookup xs "translateError" (EitherEff (RealLocated TranslateError))
  ) => T.LField -> Eff xs (Id, Type)
typeCheckField (L _ (T.Field (L _ id) _ typeid)) = (id,) <$> lookupTypeIdEff typeid
