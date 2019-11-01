module Tiger.Semant where

import           Control.Monad.Except
import qualified Data.Bifunctor as Bi
import           Data.Extensible
import           Data.Foldable
import           RIO
import qualified RIO.List as List
import qualified RIO.Partial as Partial

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
import           Tiger.Semant.TypeCheck
import           Tiger.Semant.Types


data SemantAnalysisError =
    TranslateError TranslateError
  | TypeCheckError TypeCheckError
  deriving Show


insertInitVAEnv :: forall xs f. (Lookup xs "varAccessEnv" (State (VAEnv f)), Lookup xs "label" UniqueEff) => Eff xs ()
insertInitVAEnv = mapM_ insertFunAccess initVEnv
  where
    insertFunAccess :: Id -> Eff xs ()
    insertFunAccess name = do
      label <- namedLabel name
      insertVarAccess name . FunAccess $ #label @= label <: #parent @= TopLevel <: nil
    initVEnv = [
        "print"
      , "flush"
      , "getchar"
      , "ord"
      , "chr"
      , "size"
      , "substring"
      , "concat"
      , "not"
      , "exit"
      ]

type HasTranslateEff xs f = (F.Frame f, HasEnv xs f, Lookup xs "typeCheckError" (EitherEff (RealLocated TypeCheckError)), Lookup xs "translateError" (EitherEff (RealLocated TranslateError)), Lookup xs "nestingLevel" (NestingLevelEff f), Lookup xs "temp" UniqueEff, Lookup xs "label" UniqueEff, Lookup xs "id" UniqueEff, Lookup xs "breakpoint" BreakPointEff, Lookup xs "fragment" (FragmentEff f))
runTranslateEff :: forall f xs a.
     Eff (
         ("typeEnv" >: State TEnv)
      ': ("varTypeEnv" >: State VTEnv)
      ': ("varAccessEnv" >: State (VAEnv f))
      ': ("nestingLevel" >: NestingLevelEff f)
      ': ("breakpoint" >: BreakPointEff)
      ': ("fragment" >: FragmentEff f)
      ': ("temp" >: UniqueEff)
      ': ("label" >: UniqueEff)
      ': ("id" >: UniqueEff)
      ': ("typeCheckError" >: EitherEff (RealLocated TypeCheckError))
      ': ("translateError" >: EitherEff (RealLocated TranslateError))
      ': xs) a
  -> Eff xs (Either (RealLocated SemantAnalysisError) (a, [F.ProgramFragment f]))
runTranslateEff = fmap join . (fmap (Bi.first (fmap TranslateError)) . runEitherEff @"translateError") . (fmap (Bi.first  (fmap TypeCheckError)) . runEitherEff @"typeCheckError") . runUniqueEff @"id" . runUniqueEff @"label" . runUniqueEff @"temp" . runFragmentEff . runBreakPointEff . runNestingLevelEff . evalEnvEff initTEnv

runTranslateEffWithNewLevel a = runTranslateEff $ do
  label <- newLabel
  withNewLevelEff label [] a

allocateLocalVariable :: (
    F.Frame f
  , Lookup xs "varAccessEnv" (State (VAEnv f))
  , Lookup xs "nestingLevel" (NestingLevelEff f)
  , Lookup xs "temp" UniqueEff
  ) => Id -> Bool -> Eff xs (Access f)
allocateLocalVariable id escape = do
  a <- allocateLocalOnCurrentLevel escape
  level <- fetchCurrentLevelEff
  let access = Access $ #level @= level <: #access @= a <: nil
  insertVarAccess id $ VarAccess access
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
translateInt i = (intExp i, typeCheckInt)
translateString :: (Lookup xs "label" UniqueEff, Lookup xs "fragment" (FragmentEff f)) =>  String -> Eff xs (Exp, Type)
translateString s = (, typeCheckString) <$> stringExp s
translateNil :: (Exp, Type)
translateNil = (nilExp, typeCheckNil)

translateValue :: forall f xs. (HasTranslateEff xs f) => T.LValue -> Eff xs (Exp, Type)
translateValue (L _ (T.Id lid)) = do
  ty <- typeCheckId lid
  lookupVarAccessEff lid >>= \case
    VarAccess access -> (, ty) <$> valueIdExp access
    _ -> undefined
translateValue (L loc (T.RecField lv (L _ field))) = do
  (lv, cont) <- typeCheckRecField (L loc (lv, field))
  (varExp, valueTy) <- translateValue lv
  ty <- cont valueTy
  skipName valueTy >>= \case
    TRecord r ->  case List.lookup field (r ^. #map) of
      Just ty -> do
        let i = Partial.fromJust $ List.findIndex (\(id, _) -> id == field) (r ^. #map)
        pure . (, ty) $ valueRecFieldExp @f varExp i
      Nothing -> throwEff #typeCheckError . L loc $ MissingRecordField lv ty field
    _ -> undefined
translateValue (L loc (T.ArrayIndex lv le)) = do
  (lv, cont) <- typeCheckArrayIndex (L loc (lv, le))
  (varExp, valueTy) <- translateValue lv
  (le, cont) <- cont valueTy
  (indexExp, indexTy) <- translateExp le
  ty <- cont indexTy
  pure . (, ty) $ valueArrayIndexExp @f varExp indexExp

translateBinOp :: forall f xs. HasTranslateEff xs f => RealLocated (T.LOp', T.LExp, T.LExp) -> Eff xs (Exp, Type)
translateBinOp (L loc (op, left, right)) = do
  (left, cont) <- typeCheckBinOp (L loc (op, left, right))
  (leftExp, leftTy) <- translateExp left
  (right, cont) <- cont leftTy
  (rightExp, rightTy) <- translateExp right
  ty <- cont rightTy
  if leftTy /= TString
    then (, ty) <$> binOpExp op leftExp rightExp
    else (, ty) <$> stringOpExp @f op leftExp rightExp

translateIfElse :: HasTranslateEff xs f => RealLocated (T.LExp, T.LExp, T.LExp) -> Eff xs (Exp, Type)
translateIfElse (L loc (bool, then', else')) = do
  (bool, cont) <- typeCheckIfElse (L loc (bool, then', else'))
  (boolExp, boolTy) <- translateExp bool
  (then', cont) <- cont boolTy
  (thenExp, thenTy) <- translateExp then'
  (else', cont) <- cont thenTy
  (elseExp, elseTy) <- translateExp else'
  ty <- cont elseTy
  (, ty) <$> ifElseExp boolExp thenExp elseExp

translateIfNoElse :: HasTranslateEff xs f => T.LExp -> T.LExp -> Eff xs (Exp, Type)
translateIfNoElse bool then' = do
  (bool, cont) <- typeCheckIfNoElse (bool, then')
  (boolExp, boolTy) <- translateExp bool
  (then', cont) <- cont boolTy
  (thenExp, thenTy) <- translateExp then'
  ty <- cont thenTy
  (, ty) <$> ifNoElseExp boolExp thenExp

translateRecordCreation :: forall f xs. HasTranslateEff xs f => RealLocated (LId, [T.LFieldAssign]) -> Eff xs (Exp, Type)
translateRecordCreation (L loc (typeid, fields)) = do
  (fields, cont) <- typeCheckRecordCreation (L loc (typeid, fields))
  (fieldExps, fieldsTy) <- unzip3' <$> mapM translateFieldAssign fields
  ty <- cont fieldsTy
  (, ty) <$> recordCreationExp @f fieldExps
  where
    unzip3' :: [(a, (b, c))] -> ([b], [(a, c)])
    unzip3' = foldr (\(a, (b, c)) (bs, acs) -> (b:bs, (a,c):acs)) ([], [])

    translateFieldAssign :: T.LFieldAssign -> Eff xs (Id, (Exp, Type))
    translateFieldAssign (L _ (T.FieldAssign (L _ id) e)) = (id,) <$> translateExp e

translateArrayCreation :: forall f xs. HasTranslateEff xs f => RealLocated (LId, T.LExp, T.LExp) -> Eff xs (Exp, Type)
translateArrayCreation (L loc (typeid, size, init)) = do
  (size, cont) <- typeCheckArrayCreation (L loc (typeid, size, init))
  (sizeExp, sizeTy) <- translateExp size
  (init, cont) <- cont sizeTy
  (initExp, initTy) <- translateExp init
  ty <- cont initTy
  (, ty) <$> arrayCreationExp @f sizeExp initExp

translateWhileLoop :: HasTranslateEff xs f => T.LExp -> T.LExp -> Eff xs (Exp, Type)
translateWhileLoop bool body = do
  (bool, cont) <- typeCheckWhileLoop (bool, body)
  (boolExp, boolTy) <- translateExp bool
  (body, cont) <- cont boolTy
  withBreakPoint $ do
    (bodyExp, bodyTy) <- translateExp body
    ty <- cont bodyTy
    (, ty) <$> whileLoopExp boolExp bodyExp

translateForLoop :: HasTranslateEff xs f => RealLocated (LId, Bool, T.LExp, T.LExp, T.LExp) -> Eff xs (Exp, Type)
translateForLoop (L _ (L _ id, escape, from, to, body)) = do
  access <- allocateLocalVariable id escape
  (from, cont) <- typeCheckForLoop (id, from, to, body)
  (fromExp, fromTy) <- translateExp from
  (to, cont) <- cont fromTy
  (toExp, toTy) <- translateExp to
  (body, cont) <- cont toTy
  withBreakPoint $ do
    (bodyStm, bodyTy) <- translateExp body
    ty <- cont bodyTy
    (, ty) <$> forLoopExp access fromExp toExp bodyStm

translateBreak :: (Lookup xs "breakpoint" BreakPointEff, Lookup xs "translateError" (EitherEff (RealLocated TranslateError))) => RealSrcSpan -> Eff xs (Exp, Type)
translateBreak loc = breakExp >>= \case
  Just exp -> pure (exp, typeCheckBreak)
  Nothing -> throwEff #translateError $ L loc BreakOutsideLoop

translateFunApply :: HasTranslateEff xs f => RealLocated (LId, [T.LExp]) -> Eff xs (Exp, Type)
translateFunApply (L loc (func, args)) = do
  (args, cont) <- typeCheckFunApply (L loc (func, args))
  (exps, argsTy) <- List.unzip <$> mapM (traverse skipName <=< translateExp) args
  ty <- cont argsTy
  lookupVarAccessEff func >>= \case
    FunAccess r -> (, ty) <$> funApplyExp (r ^. #label) (r ^. #parent) exps
    VarAccess _ -> undefined

translateAssign :: HasTranslateEff xs f => T.LValue -> T.LExp -> Eff xs (Exp, Type)
translateAssign v e = do
  (v, cont) <- typeCheckAssign (v, e)
  (varExp, varTy) <- translateValue v
  (e, cont) <- cont varTy
  (exp, expTy) <- translateExp e
  ty <- cont expTy
  (, ty) <$> assignExp varExp exp

translateSeq :: HasTranslateEff xs f => [T.LExp] -> Eff xs (Exp, Type)
translateSeq es = do
  (es, cont) <- typeCheckSeq es
  (exps, types) <- List.unzip <$> mapM translateExp es
  ty <- cont types
  (, ty) <$> seqExp exps

translateLet :: HasTranslateEff xs f => [T.LDec] -> T.LExp -> Eff xs (Exp, Type)
translateLet decs body =
  withVAEnvScope $ do
    (decsList, cont) <- typeCheckLet (decs, body)
    exps <- translateDecsList decsList
    (body, cont) <- cont ()
    (bodyExp, bodyTy) <- translateExp body
    ty <- cont bodyTy
    pure (letExp exps bodyExp, ty)

translateDecsList :: forall f xs. HasTranslateEff xs f => [Decs] -> Eff xs [Exp]
translateDecsList = fmap mconcat . traverse translateDecs
  where
    translateDecs (VarDecs ds) = traverse translateVarDec ds
    translateDecs (FunDecs ds) = translateFunDecs ds >> pure []
    translateDecs (TypeDecs ds) = typeCheckTypeDecs ds >> pure []

    translateVarDec :: RealLocated VarDec -> Eff xs Exp
    translateVarDec (L loc (VarDec r)) = do
      (init, cont) <- typeCheckVarDec (L loc (VarDec r))
      (initExp, initTy) <- translateExp init
      cont initTy
      access <- allocateLocalVariable (unLId $ r ^. #id) (r ^. #escape)
      varInitExp access initExp

    translateFunDecs :: [RealLocated FunDec] -> Eff xs ()
    translateFunDecs ds = do
      typeCheckFunDecs ds
      mapM_ insertFunAccess ds
      mapM_ translateFunDec ds

    insertFunAccess :: RealLocated FunDec -> Eff xs ()
    insertFunAccess (L _ (FunDec r)) = do
      label <- namedLabel . unLId $ r ^. #id
      parent <- fetchCurrentLevelEff
      insertVarAccess (unLId $ r ^. #id) . FunAccess $ #label @= label <: #parent @= parent <: nil

    translateFunDec :: forall xs. (HasTranslateEff xs f) => RealLocated FunDec -> Eff xs ()
    translateFunDec (L loc (FunDec dec)) = do
      (body, cont) <- typeCheckFunDec (L loc (FunDec dec))
      lookupVarAccessEff (dec ^. #id) >>= \case
        VarAccess _ -> undefined
        FunAccess f -> withNewLevelEff (f ^. #label) escapes $ do
          insertFormals . fmap extractLId $ dec ^. #args
          (bodyExp, bodyTy) <- translateExp body
          cont bodyTy
          funDecExp bodyExp
      where
        extractLId (L _ (T.Field (L _ id) _ _)) = id
        escapes = (\(L _ (T.Field _ escape _)) -> escape) <$> dec ^. #args
        insertFormals :: [Id] -> Eff xs ()
        insertFormals args = do
          formals <- fetchCurrentLevelParametersAccessEff
          zipWithM_ insertFormal args formals
        insertFormal :: Id -> F.Access f -> Eff xs ()
        insertFormal id a = do
          level <- fetchCurrentLevelEff
          let access = Access $ #level @= level <: #access @= a <: nil
          insertVarAccess id $ VarAccess access
