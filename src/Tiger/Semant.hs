module Tiger.Semant where

import           Control.Monad.Except
import           Control.Lens ((.~))
import           Data.Extensible
import           Data.Extensible.Effect.Default
import           Data.Foldable
import           Data.Graph
import           RIO
import qualified RIO.List as List
import qualified RIO.Partial as Partial
import qualified RIO.Set as Set

import qualified Env as E
import qualified Frame as F
import           Id
import           SrcLoc
import           Unique

import qualified Tiger.LSyntax as T
import           Tiger.Semant.BreakPoint
import           Tiger.Semant.Env
import           Tiger.Semant.Exp
import           Tiger.Semant.Level
import           Tiger.Semant.Translate
import           Tiger.Semant.Types



initTEnv :: TEnv
initTEnv = E.fromList [("string", TString), ("int", TInt)]
initVEnv :: VEnv f
initVEnv = E.fromList []
-- initVEnv = E.fromList [
--     ("print", Fun $ #label @= undefined <: #level @= TopLevel <: #parent @= undefined <: #domains @= [TString] <: #codomain @= TUnit <: nil)
--   , ("flush", Fun $ #label @= undefined <: #level @= TopLevel <: #parent @= undefined <: #domains @= [] <: #codomain @= TUnit <: nil)
--   , ("getchar", Fun $ #label @= undefined <: #level @= TopLevel <: #parent @= undefined <: #domains @= [] <: #codomain @= TString <: nil)
--   , ("ord", Fun $ #label @= undefined <: #level @= TopLevel <: #parent @= undefined <: #domains @= [TString] <: #codomain @= TInt <: nil)
--   , ("chr", Fun $ #label @= undefined <: #level @= TopLevel <: #parent @= undefined <: #domains @= [TInt] <: #codomain @= TString <: nil)
--   , ("size", Fun $ #label @= undefined <: #level @= TopLevel <: #parent @= undefined <: #domains @= [TString] <: #codomain @= TInt <: nil)
--   , ("substring", Fun $ #label @= undefined <: #level @= TopLevel <: #parent @= undefined <: #domains @= [TString, TInt, TInt] <: #codomain @= TString <: nil)
--   , ("concat", Fun $ #label @= undefined <: #level @= TopLevel <: #parent @= undefined <: #domains @= [TString, TString] <: #codomain @= TString <: nil)
--   , ("not", Fun $ #label @= undefined <: #level @= TopLevel <: #parent @= undefined <: #domains @= [TInt] <: #codomain @= TInt <: nil)
--   , ("exit", Fun $ #label @= undefined <: #level @= TopLevel <: #parent @= undefined <: #domains @= [TInt] <: #codomain @= TUnit <: nil)
--   ]

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


type HasTranslateEff xs f = (F.Frame f, HasEnv xs f, Lookup xs "translateError" (EitherEff (RealLocated TranslateError)), Lookup xs "nestingLevel" (NestingLevelEff f), Lookup xs "temp" UniqueEff, Lookup xs "label" UniqueEff, Lookup xs "id" UniqueEff, Lookup xs "breakpoint" BreakPointEff, Lookup xs "fragment" (FragmentEff f))
runTranslateEff :: forall f xs a.
     Eff (
         ("typeEnv" >: State TEnv)
      ': ("varEnv" >: State (VEnv f))
      ': ("nestingLevel" >: NestingLevelEff f)
      ': ("breakpoint" >: BreakPointEff)
      ': ("fragment" >: FragmentEff f)
      ': ("temp" >: UniqueEff)
      ': ("label" >: UniqueEff)
      ': ("id" >: UniqueEff)
      ': ("translateError" >: EitherEff (RealLocated TranslateError))
      ': xs) a
  -> Eff xs (Either (RealLocated TranslateError) (a, [F.ProgramFragment f]))
runTranslateEff = runEitherEff @"translateError" . runUniqueEff @"id" . runUniqueEff @"label" . runUniqueEff @"temp" . runFragmentEff . runBreakPointEff . runNestingLevelEff . evalEnvEff initTEnv initVEnv

runTranslateEffWithNewLevel a = runTranslateEff $ do
  label <- newLabel
  withNewLevelEff label [] a

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
    pure . TArray $ arr & #range .~ ty
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

allocateLocalVariable :: (F.Frame f, Lookup xs "varEnv" (State (VEnv f)), Lookup xs "nestingLevel" (NestingLevelEff f), Lookup xs "temp" UniqueEff) => Id -> Bool -> Type -> Eff xs (Access f)
allocateLocalVariable id escape ty = do
  a <- allocateLocalOnCurrentLevel escape
  level <- fetchCurrentLevelEff
  let access = Access $ #level @= level <: #access @= a <: nil
  insertVar id . Var $ #type @= ty <: #access @= access <: nil
  pure access


translateExp :: forall f xs. HasTranslateEff xs f => T.LExp -> Eff xs (Exp, Type)
translateExp (L _ (T.Int i)) = pure $ translateInt i
translateExp (L _ (T.String s)) = translateString s
translateExp (L _ T.Nil) = pure translateNil
translateExp (L _ (T.Var v)) = translateValue v
translateExp (L loc (T.Op left (L _ op) right)) = translateBinOp $ L loc (op, left, right)
translateExp (L loc (T.If bool then' (Just else'))) = translateIfElse $ L loc (bool, then', else')
translateExp (L _ (T.If bool then' Nothing)) = translateIfNoElse bool then'
translateExp (L loc (T.RecordCreate typeid fields)) = translateRecordCreation @f $ L loc (typeid, fields)
translateExp (L loc (T.ArrayCreate typeid size init)) = translateArrayCreation @f $ L loc (typeid, size, init)
translateExp (L _ (T.Assign v e)) = translateAssign v e
translateExp (L _ (T.Seq es)) = translateSeq es
translateExp (L _ (T.While bool body)) = translateWhileLoop bool body
translateExp (L loc T.Break) = translateBreak loc
translateExp (L loc (T.For lid escape from to body)) = translateForLoop $ L loc (lid, escape, from, to, body)
translateExp (L loc (T.FunApply func args)) = translateFunApply $ L loc (func, args)
translateExp (L _ (T.Let decs body)) = translateLet decs body

translateInt :: Int -> (Exp, Type)
translateInt i = (intExp i, TInt)
translateString :: (Lookup xs "label" UniqueEff, Lookup xs "fragment" (FragmentEff f)) =>  String -> Eff xs (Exp, Type)
translateString s = (, TString) <$> stringExp s
translateNil :: (Exp, Type)
translateNil = (nilExp, TNil)

translateValue :: forall f xs. (HasTranslateEff xs f) => T.LValue -> Eff xs (Exp, Type)
translateValue (L loc (T.Id lid)) = do
  var <- lookupVarIdEff lid
  case var of
    Var r -> (, r ^. #type) <$> valueIdExp (r ^. #access)
    Fun _ -> throwEff #translateError . L loc $ ExpectedVariable (unLId lid)
translateValue (L loc (T.RecField lv (L _ field))) = do
  (varExp, ty) <- traverse skipName =<< translateValue lv
  case ty of
    TRecord r -> case List.lookup field (r ^. #map) of
      Just ty -> do
        let i = Partial.fromJust $ List.findIndex (\(id, _) -> id == field) (r ^. #map)
        pure . (, ty) $ valueRecFieldExp @f varExp i
      Nothing -> throwEff #translateError . L loc $ MissingRecordField lv ty field
    _ -> throwEff #translateError . L loc $ ExpectedRecordType lv ty
translateValue (L loc (T.ArrayIndex lv le)) = do
  (varExp, ty) <- traverse skipName =<< translateValue lv
  case ty of
    TArray a -> do
      (indexExp, indexTy) <- translateExp le
      checkInt indexTy le
      pure . (, a ^. #range) $ valueArrayIndexExp @f varExp indexExp
    _ -> throwEff #translateError . L loc $ ExpectedArrayType lv ty

translateBinOp :: forall f xs. HasTranslateEff xs f => RealLocated (T.LOp', T.LExp, T.LExp) -> Eff xs (Exp, Type)
translateBinOp (L loc (op, left, right)) = do
  (leftExp, leftTy) <- translateExp left
  (rightExp, rightTy) <- translateExp right
  typecheck op leftTy left rightTy right
  if leftTy /= TString
    then (, TInt) <$> binOpExp op leftExp rightExp
    else (, TInt) <$> stringOpExp @f op leftExp rightExp
  where
    isEqNEq T.Eq = True
    isEqNEq T.NEq = True
    isEqNEq _ = False

    isUnit TUnit = True
    isUnit _ = False

    typecheck op leftTy left rightTy right
      | not (isEqNEq op) = checkInt leftTy left >> checkInt rightTy right
      | isUnit leftTy = throwEff #translateError . L loc $ ExpectedExpression left
      | isUnit rightTy = throwEff #translateError . L loc $ ExpectedExpression right
      | leftTy == TNil && rightTy == TNil = throwEff #translateError . L loc $ NotDeterminedNilType
      | not (isComparable leftTy rightTy) = throwEff #translateError . L loc $ ExpectedType right leftTy rightTy
      | otherwise = pure ()

translateIfElse :: HasTranslateEff xs f => RealLocated (T.LExp, T.LExp, T.LExp) -> Eff xs (Exp, Type)
translateIfElse (L loc (bool, then', else')) = do
  (boolExp, boolTy) <- translateExp bool
  checkInt boolTy bool
  (thenExp, thenTy) <- translateExp then'
  (elseExp, elseTy) <- translateExp else'
  if isComparable thenTy elseTy
    then (, thenTy) <$> ifElseExp boolExp thenExp elseExp
    else throwEff #translateError . L loc $ ExpectedType else' thenTy elseTy

translateIfNoElse :: HasTranslateEff xs f => T.LExp -> T.LExp -> Eff xs (Exp, Type)
translateIfNoElse bool then' = do
  (boolExp, boolTy) <- translateExp bool
  checkInt boolTy bool
  (thenExp, thenTy) <- translateExp then'
  checkUnit thenTy then'
  (, TUnit) <$> ifNoElseExp boolExp thenExp

translateRecordCreation :: forall f xs. HasTranslateEff xs f => RealLocated (LId, [T.LFieldAssign]) -> Eff xs (Exp, Type)
translateRecordCreation (L loc (typeid, fields)) = do
  ty <- lookupTypeIdEff typeid
  case ty of
    TRecord r -> do
      (fieldsty, fieldExps) <- unzip3' <$> mapM translateFieldAssign fields
      typecheck ty (r ^. #map) fieldsty
      (, ty) <$> recordCreationExp @f fieldExps
    _ -> throwEff #translateError . L loc $ ExpectedRecordType (L loc (T.Id typeid)) ty
  where
    typecheck :: Type -> [(Id, Type)] -> [(Id, Type)] -> Eff xs ()
    typecheck _ [] [] = pure ()
    typecheck ty ((id1, _):_) [] = throwEff #translateError . L loc $ MissingRecordFieldInConstruction (L loc $ T.RecordCreate typeid fields) ty id1
    typecheck ty [] ((id2, _):_) = throwEff #translateError . L loc $ ExtraRecordFieldInConstruction (L loc $ T.RecordCreate typeid fields) ty id2
    typecheck ty ((id1, ty1):as) ((id2, ty2):bs)
      | id1 < id2 = throwEff #translateError . L loc $ MissingRecordFieldInConstruction (L loc $ T.RecordCreate typeid fields) ty id1
      | id1 > id2 = throwEff #translateError . L loc $ ExtraRecordFieldInConstruction (L loc $ T.RecordCreate typeid fields) ty id2
      |  ty1 > ty2 = throwEff #translateError . L loc $ ExpectedTypeForRecordField (L loc $ T.RecordCreate typeid fields) id1 ty1 ty2
      | otherwise = typecheck ty as bs

    unzip3' :: [(a, (b, c))] -> ([(a, c)], [b])
    unzip3' = foldr (\(a, (b, c)) (acs, bs) -> ((a,c):acs, b:bs)) ([], [])

translateFieldAssign :: HasTranslateEff xs f => T.LFieldAssign -> Eff xs (Id, (Exp, Type))
translateFieldAssign (L _ (T.FieldAssign (L _ id) e)) = (id,) <$> translateExp e

translateArrayCreation :: forall f xs. HasTranslateEff xs f => RealLocated (LId, T.LExp, T.LExp) -> Eff xs (Exp, Type)
translateArrayCreation (L loc (typeid, size, init)) = lookupSkipName typeid >>= \case
  ty@(TArray a) -> do
    (sizeExp, sizeTy) <- translateExp size
    checkInt sizeTy size
    (initExp, initty) <- translateExp init
    if a ^. #range <= initty
      then (, ty) <$> arrayCreationExp @f sizeExp initExp
      else throwEff #translateError . L loc $ ExpectedType init (a ^. #range) initty
  ty -> throwEff #translateError . L loc $ ExpectedArrayType (L loc (T.Id typeid)) ty

translateWhileLoop :: HasTranslateEff xs f => T.LExp -> T.LExp -> Eff xs (Exp, Type)
translateWhileLoop bool body = do
  (boolExp, boolTy) <- translateExp bool
  checkInt boolTy bool
  withBreakPoint $ do
    (bodyExp, bodyTy) <- translateExp body
    checkUnit bodyTy body
    (, TUnit) <$> whileLoopExp boolExp bodyExp

translateForLoop :: HasTranslateEff xs f => RealLocated (LId, Bool, T.LExp, T.LExp, T.LExp) -> Eff xs (Exp, Type)
translateForLoop (L _ (L _ id, escape, from, to, body)) = do
  access <- allocateLocalVariable id escape TInt
  (fromExp, fromTy) <- translateExp from
  checkInt fromTy from
  (toExp, toTy) <- translateExp to
  checkInt toTy to
  withBreakPoint $ do
    (bodyStm, bodyTy) <- translateExp body
    checkUnit bodyTy body
    (, TUnit) <$> forLoopExp access fromExp toExp bodyStm

translateBreak :: (Lookup xs "breakpoint" BreakPointEff, Lookup xs "translateError" (EitherEff (RealLocated TranslateError))) => RealSrcSpan -> Eff xs (Exp, Type)
translateBreak loc = breakExp >>= \case
  Just exp -> pure (exp, TUnit)
  Nothing -> throwEff #translateError $ L loc BreakOutsideLoop

translateFunApply :: HasTranslateEff xs f => RealLocated (LId, [T.LExp]) -> Eff xs (Exp, Type)
translateFunApply (L loc (func, args)) = lookupVarIdEff func >>= \case
  Fun r -> do
    (exps, argsty) <- List.unzip <$> mapM (traverse skipName <=< translateExp) args
    domains <- mapM skipName $ r ^. #domains
    if domains <= argsty
      then do
        exp <- funApplyExp (r ^. #label) (r ^. #parent) exps
        (exp, ) <$> skipName (r ^. #codomain)
      else throwEff #translateError . L loc $ ExpectedTypes args domains argsty
  Var _ -> throwEff #translateError . L loc $ ExpectedFunction (unLId func)

translateAssign :: HasTranslateEff xs f => T.LValue -> T.LExp -> Eff xs (Exp, Type)
translateAssign v e@(L loc _) = do
  (varExp, varTy) <- translateValue v
  (exp, expTy) <- translateExp e
  if varTy <= expTy
    then (, TUnit) <$> assignExp varExp exp
    else throwEff #translateError . L loc $ ExpectedType e varTy expTy

translateSeq :: HasTranslateEff xs f => [T.LExp] -> Eff xs (Exp, Type)
translateSeq es = do
  (exps, types) <- List.unzip <$> mapM translateExp es
  case List.lastMaybe types of
    Just ty -> (, ty) <$> seqExp exps
    Nothing -> pure (unitExp, TUnit)

translateLet :: HasTranslateEff xs f => [T.LDec] -> T.LExp -> Eff xs (Exp, Type)
translateLet decs body =
  withTEnvScope . withVEnvScope $ do
    exps <- translateDecsList $ groupByDecType decs
    (exp, ty) <- translateExp body
    pure (letExp exps exp, ty)

newtype FunDec = FunDec (Record '["id" >: LId, "args" >: [T.LField], "rettype" >: Maybe LId, "body" >: T.LExp])
newtype VarDec = VarDec (Record '["id" >: LId, "escape" >: Bool, "type" >: Maybe LId, "init" >: T.LExp])
newtype TypeDec = TypeDec (Record '["id" >: LId, "type" >: T.LType]) deriving Show
data Decs = FunDecs [RealLocated FunDec] | VarDecs [RealLocated VarDec] | TypeDecs [RealLocated TypeDec]
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

translateDecsList :: forall f xs. HasTranslateEff xs f => [Decs] -> Eff xs [Exp]
translateDecsList = fmap mconcat . traverse translateDecs
  where
    translateDecs (VarDecs ds) = traverse translateVarDec ds
    translateDecs (FunDecs ds) = translateFunDecs ds >> pure []
    translateDecs (TypeDecs ds) = translateTypeDecs ds >> pure []


    translateVarDec :: RealLocated VarDec -> Eff xs Exp
    translateVarDec (L loc (VarDec r)) = do
      (initExp, initTy) <- translateExp $ r ^. #init
      typecheck (r ^. #type) initTy (r ^. #init)
      ty <- maybe (pure initTy) lookupSkipName $ r ^. #type
      access <- allocateLocalVariable (unLId $ r ^. #id) (r ^. #escape) ty
      varInitExp access initExp
      where
        typecheck (Just typeid) initTy init = do
          declaredTy <- lookupSkipName typeid
          unless (declaredTy <= initTy) . throwEff #translateError . L loc $ ExpectedType init declaredTy initTy -- opposite to subtyping
        typecheck Nothing initTy _ =
          when (initTy == TNil) . throwEff #translateError . L loc $ NotDeterminedNilType


    translateFunDecs :: [RealLocated FunDec] -> Eff xs ()
    translateFunDecs ds = do
      checkSameNameDec $ fmap extractLId ds
      mapM_ insertFunEntry ds
      mapM_ translateFunDec ds
      where
        extractLId (L _ (FunDec r)) = r ^. #id

    insertFunEntry :: RealLocated FunDec -> Eff xs ()
    insertFunEntry (L _ (FunDec r)) = do
      label <- namedLabel . unLId $ r ^. #id
      parent <- fetchCurrentLevelEff
      codomain <- maybe (pure TUnit) lookupTypeIdEff $ r ^. #rettype
      let fun = Fun $ #label @= label <: #parent @= parent <: #domains @= domains <: #codomain @= codomain <: nil
      insertVar (unLId $ r ^. #id) fun
      where
        domains = TName . (\(L _ (T.Field _ _ typeid)) -> typeid)  <$> r ^. #args

    translateFunDec :: forall xs. (HasTranslateEff xs f) => RealLocated FunDec -> Eff xs ()
    translateFunDec (L loc (FunDec dec)) = lookupVarIdEff (dec ^. #id) >>= \case
      Var _ -> undefined
      Fun f -> withNewLevelEff (f ^. #label) escapes $ do
        insertFormals $ dec ^. #args
        (bodyExp, bodyTy) <- translateExp $ dec ^. #body
        declaredTy <- maybe (pure TUnit) lookupSkipName $ dec ^. #rettype
        if declaredTy <= bodyTy
          then funDecExp bodyExp
          else throwEff #translateError . L loc $ ExpectedType (dec ^. #body) declaredTy bodyTy
      where
        escapes = (\(L _ (T.Field _ escape _)) -> escape) <$> dec ^. #args
        insertFormals :: [T.LField] -> Eff xs ()
        insertFormals args = do
          formals <- fetchCurrentLevelParametersAccessEff
          zipWithM_ insertFormal args formals
        insertFormal :: T.LField -> F.Access f -> Eff xs ()
        insertFormal (L _ (T.Field (L _ id) _ (L loc typeid))) a = do
          level <- fetchCurrentLevelEff
          let access = Access $ #level @= level <: #access @= a <: nil
          lookupTypeId typeid >>= \case
            Just ty -> insertVar id . Var $ #type @= ty <: #access @= access <: nil
            Nothing -> throwEff #translateError . L loc $ UnknownType typeid

    translateTypeDecs :: [RealLocated TypeDec] -> Eff xs ()
    translateTypeDecs ds = do
      let typeLIds = fmap extractLId ds
      checkSameNameDec typeLIds
      checkInvalidRecType ds
      types <- withTEnvScope $ do
        mapM_ (\lid -> insertType (unLId lid) (TName lid)) typeLIds
        mapM (\(L _ (TypeDec r)) -> (unLId (r ^. #id), ) <$> typingType (r ^. #type)) ds
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


typingType :: (
    Lookup xs "typeEnv" (State TEnv)
  , Lookup xs "translateError" (EitherEff (RealLocated TranslateError))
  , Lookup xs "id" UniqueEff
  ) => T.LType -> Eff xs Type
typingType (L _ (T.TypeId typeid)) = lookupTypeIdEff typeid
typingType (L _ (T.RecordType fields)) = do
  fieldmap <- foldrM (\field e -> (\(id, ty) -> (:) (id, ty) e) <$> typingField field) [] fields
  id <- getUniqueEff #id
  pure . TRecord $ #map @= fieldmap <: #id @= id <: nil
typingType (L _ (T.ArrayType typeid)) = do
  ty <- lookupTypeIdEff typeid
  id <- getUniqueEff #id
  pure . TArray $ #range @= ty <: #id @= id <: nil

typingField :: (
    Lookup xs "typeEnv" (State TEnv)
  , Lookup xs "translateError" (EitherEff (RealLocated TranslateError))
  ) => T.LField -> Eff xs (Id, Type)
typingField (L _ (T.Field (L _ id) _ typeid)) = (id,) <$> lookupTypeIdEff typeid
