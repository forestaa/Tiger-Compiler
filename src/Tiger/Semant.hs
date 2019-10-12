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
import Data.Graph

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
    ("print", Fun $ #label @= undefined <: #level @= TopLevel <: #domains @= [TString] <: #codomain @= TUnit <: nil)
  , ("flush", Fun $ #label @= undefined <: #level @= TopLevel <: #domains @= [] <: #codomain @= TUnit <: nil)
  , ("getchar", Fun $ #label @= undefined <: #level @= TopLevel <: #domains @= [] <: #codomain @= TString <: nil)
  , ("ord", Fun $ #label @= undefined <: #level @= TopLevel <: #domains @= [TString] <: #codomain @= TInt <: nil)
  , ("chr", Fun $ #label @= undefined <: #level @= TopLevel <: #domains @= [TInt] <: #codomain @= TString <: nil)
  , ("size", Fun $ #label @= undefined <: #level @= TopLevel <: #domains @= [TString] <: #codomain @= TInt <: nil)
  , ("substring", Fun $ #label @= undefined <: #level @= TopLevel <: #domains @= [TString, TInt, TInt] <: #codomain @= TString <: nil)
  , ("concat", Fun $ #label @= undefined <: #level @= TopLevel <: #domains @= [TString, TString] <: #codomain @= TString <: nil)
  , ("not", Fun $ #label @= undefined <: #level @= TopLevel <: #domains @= [TInt] <: #codomain @= TInt <: nil)
  , ("exit", Fun $ #label @= undefined <: #level @= TopLevel <: #domains @= [TInt] <: #codomain @= TUnit <: nil)
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
  | ExpectedVariable Id
  | ExpectedExpression T.LExp
  | ExpectedTypeForRecordField T.LExp Id Type Type
  | MissingRecordField T.LValue Type Id
  | MissingRecordFieldInConstruction T.LExp Type Id
  | ExtraRecordFieldInConstruction T.LExp Type Id
  | InvalidRecTypeDeclaration [RealLocated TypeDec]
  | MultiDeclaredName [LId]
  | AssignNilWithNoRecordAnnotation

  -- translate
  | BreakOutsideLoop

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
  show (ExpectedVariable id) = concat ["Expected Variable: value = ", show id]
  show (ExpectedExpression (L _ e)) = concat ["Expected Expression: ", show e]
  show (ExpectedTypeForRecordField (L _ e) id ty ty') = concat ["Couldn't match type: ", show ty, " type expected at field ", show id, ": exp = ", show e, ", actual type: ", show ty']
  show (MissingRecordField (L _ v) ty id) = concat ["Missing record field: value = ", show v, ", type = ", show ty, ", field = ", show id]
  show (MissingRecordFieldInConstruction (L _ v) ty id) = concat ["Missing record field in construction: value = ", show v, ", type = ", show ty, ", field = ", show id]
  show (ExtraRecordFieldInConstruction (L _ v) ty id) = concat ["Record does not have field ", show id, ": value = ", show v, ", type = ", show ty]
  show (InvalidRecTypeDeclaration decs) = concat ["Found circle type declarations: decs = ", show decs]
  show (MultiDeclaredName decs) = concat ["Same name types, vars or functions declared: decs = ", show decs]
  show AssignNilWithNoRecordAnnotation = "nil assigned with no type annotation"

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
    TRecord r -> case Map.lookup field (r ^. #map) of
      Just ty -> do
        let i = Partial.fromJust $ Map.lookupIndex field (r ^. #map)
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
      | isEqNEq op && isUnit leftTy = throwEff #translateError . L loc $ ExpectedExpression left
      | isEqNEq op && isUnit rightTy = throwEff #translateError . L loc $ ExpectedExpression right
      | isEqNEq op && not (isComparable leftTy rightTy) = throwEff #translateError . L loc $ ExpectedType right leftTy rightTy
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
      let m = Map.toList $ r ^. #map
      (fieldsty, fieldExps) <- unzip3' <$> mapM translateFieldAssign fields
      typecheck ty m fieldsty
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
      |  ty1 /= ty2 = throwEff #translateError . L loc $ ExpectedTypeForRecordField (L loc $ T.RecordCreate typeid fields) id1 ty1 ty2
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
    if initty == a ^. #range
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
    if argsty == domains
      then do
        exp <- funApplyExp (r ^. #label) (r ^. #level) exps
        (exp, ) <$> skipName (r ^. #codomain)
      else throwEff #translateError . L loc $ ExpectedTypes args domains argsty
  Var _ -> throwEff #translateError . L loc $ NotImplemented "2"

translateAssign :: HasTranslateEff xs f => T.LValue -> T.LExp -> Eff xs (Exp, Type)
translateAssign v e = do
  (varExp, _) <- translateValue v
  (exp, _) <- translateExp e
  pure (assignExp varExp exp, TUnit)

translateSeq :: HasTranslateEff xs f => [T.LExp] -> Eff xs (Exp, Type)
translateSeq es = do
  (exps, types) <- List.unzip <$> mapM translateExp es
  case List.lastMaybe types of
    Just ty -> (, ty) <$> seqExp exps

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
    convertVarDec (L loc (T.VarDec id escape ty init)) = L loc . VarDec $ #id @= id <: #escape @= escape <: #type @= ty <: #init @= init <: nil
    convertTypeDec (L loc (T.TypeDec id ty)) = L loc . TypeDec $ #id @= id <: #type @= ty <: nil

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
      typecheck (r ^. #type) initTy
      ty <- maybe (pure TNil) lookupSkipName $ r ^. #type
      access <- allocateLocalVariable (unLId $ r ^. #id) (r ^. #escape) ty
      varInitExp access initExp
      where
        typecheck (Just typeid) initTy = do
          declaredTy <- lookupSkipName typeid
          unless (declaredTy <= initTy) . throwEff #translateError . L loc $ VariableMismatchedWithDeclaredType (unLId $ r ^. #id) declaredTy initTy -- opposite to subtyping
        typecheck Nothing initTy =
          when (initTy == TNil) . throwEff #translateError . L loc $ AssignNilWithNoRecordAnnotation


    translateFunDecs :: [RealLocated FunDec] -> Eff xs ()
    translateFunDecs ds = do
      mapM_ insertFunEntry ds
      mapM_ translateFunDec ds

    insertFunEntry :: RealLocated FunDec -> Eff xs ()
    insertFunEntry (L _ (FunDec r)) = do
      label <- newLabel
      withNewLevelEff label escapes $ do
        level <- fetchCurrentLevelEff
        codomain <- maybe (pure TUnit) lookupTypeIdEff $ r ^. #rettype
        let fun = Fun $ #label @= label <: #level @= level <: #domains @= domains <: #codomain @= codomain <: nil
        insertVar (unLId $ r ^. #id) fun
      where
        escapes = (\(L _ (T.Field _ escape _)) -> escape) <$> r ^. #args
        domains = TName . (\(L _ (T.Field _ _ typeid)) -> typeid)  <$> r ^. #args

    translateFunDec :: forall xs. (HasTranslateEff xs f) => RealLocated FunDec -> Eff xs ()
    translateFunDec (L loc (FunDec dec)) = lookupVarIdEff (dec ^. #id) >>= \case
      Fun f -> withLevelEff (f ^. #level) $ do
        allocateParameters $ dec ^. #args
        (bodyExp, bodyTy) <- translateExp $ dec ^. #body
        declaredTy <- maybe (pure TUnit) lookupSkipName $ dec ^. #rettype
        if declaredTy == bodyTy
          then funDecExp bodyExp
          else throwEff #translateError . L loc $ ExpectedType (dec ^. #body) declaredTy bodyTy
      where
        allocateParameters :: [T.LField] -> Eff xs ()
        allocateParameters = mapM_ allocateParameter
        allocateParameter :: T.LField -> Eff xs ()
        allocateParameter (L _ (T.Field (L _ id) escape (L loc typeid))) =
          lookupTypeId typeid >>= \case
            Just ty -> allocateLocalVariable id escape ty >> pure ()
            Nothing -> throwEff #translateError . L loc $ TypeUndefined typeid


    translateTypeDecs :: [RealLocated TypeDec] -> Eff xs ()
    translateTypeDecs ds = do
      checkSameNameDec $ fmap extractLId ds
      checkInvalidRecType ds
      mapM_ translateTypeDec ds
      where
        extractLId (L _ (TypeDec r)) = r ^. #id

    translateTypeDec :: RealLocated TypeDec -> Eff xs ()
    translateTypeDec (L _ (TypeDec r)) =
      typingType (r ^. #type) >>= insertType (unLId $ r ^. #id)

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
  fieldmap <- foldM (\e field -> (\(id, ty) -> Map.insert id ty e) <$> typingField field) Map.empty fields
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


