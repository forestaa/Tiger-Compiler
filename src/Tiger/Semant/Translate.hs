module Tiger.Semant.Translate where

import           Data.Extensible
import           RIO
import qualified RIO.List as List

import qualified Frame as F
import qualified IR
import           Unique

import qualified Tiger.LSyntax as T
import           Tiger.Semant.BreakPoint
import           Tiger.Semant.Env
import           Tiger.Semant.Exp
import           Tiger.Semant.Level



type FragmentEff f = State [F.ProgramFragment f]
runFragmentEff :: Eff (("fragment" >: FragmentEff f) ': xs) a -> Eff xs (a, [F.ProgramFragment f])
runFragmentEff = flip (runStateEff @"fragment") []
saveProcEntry :: (F.Frame f, Lookup xs "fragment" (FragmentEff f)) => Level f -> IR.Stm -> Eff xs ()
saveProcEntry TopLevel _ = undefined
saveProcEntry (Level r) stm = modifyEff #fragment . (:) . F.Proc $ #body @= stm <: #frame @= r ^. #frame <: nil
saveStringEntry :: Lookup xs "fragment" (FragmentEff f) => Label -> String -> Eff xs ()
saveStringEntry label s = modifyEff #fragment . (:) $ F.String label s


intExp :: Int -> Exp
intExp i = Ex $ IR.Const i

stringExp :: (Lookup xs "label" UniqueEff, Lookup xs "fragment" (FragmentEff f)) => String -> Eff xs Exp
stringExp s = do
  label <- newLabel
  saveStringEntry label s
  pure . Ex $ IR.Name label

nilExp :: Exp
nilExp = Ex $ IR.Const 0

unitExp :: Exp
unitExp = Nx $ IR.Exp (IR.Const 0)

valueIdExp :: (
    F.Frame f
  , Lookup xs "nestingLevel" (NestingLevelEff f)
  ) => Access f -> Eff xs Exp
valueIdExp (Access r) = Ex . F.exp (r ^. #access) <$> pullInStaticLinksEff (r ^. #level)
valueRecFieldExp :: forall f. F.Frame f => Exp -> Int -> Exp
valueRecFieldExp (Ex recordVarExp) fieldNumber = Ex $ IR.Mem (IR.BinOp IR.Plus recordVarExp (IR.Const (fieldNumber * F.wordSize @f)))
valueRecFieldExp _ _ = undefined
valueArrayIndexExp :: forall f. F.Frame f => Exp -> Exp -> Exp
valueArrayIndexExp (Ex arrayVarExp) (Ex indexExp) = Ex $ IR.Mem (IR.BinOp IR.Plus arrayVarExp (IR.BinOp IR.Mul indexExp (IR.Const (F.wordSize @f))))
valueArrayIndexExp _ _ = undefined

binOpExp :: (Lookup xs "label" UniqueEff, Lookup xs "temp" UniqueEff) => T.LOp' -> Exp -> Exp -> Eff xs Exp
binOpExp op left right
  | isArithmetic op = arithmeticOpExp (arithmeticOpConvert op) left right
  | otherwise = condOpExp (relOpConvert op) left right
  where
    isArithmetic T.Plus = True
    isArithmetic T.Minus = True
    isArithmetic T.Times = True
    isArithmetic T.Div = True
    isArithmetic _ = False

    arithmeticOpConvert T.Plus  = IR.Plus
    arithmeticOpConvert T.Minus = IR.Minus
    arithmeticOpConvert T.Times = IR.Mul
    arithmeticOpConvert T.Div   = IR.Div
    arithmeticOpConvert _       = undefined

    relOpConvert T.Eq    = IR.Eq
    relOpConvert T.NEq   = IR.Ne
    relOpConvert T.Lt    = IR.Lt
    relOpConvert T.Le    = IR.Le
    relOpConvert T.Gt    = IR.Gt
    relOpConvert T.Ge    = IR.Ge
    relOpConvert _       = undefined

arithmeticOpExp :: (Lookup xs "label" UniqueEff, Lookup xs "temp" UniqueEff) => IR.BinOp -> Exp -> Exp -> Eff xs Exp
arithmeticOpExp op left right = do
  lefte <- unEx left
  righte <- unEx right
  pure . Ex $ IR.BinOp op lefte righte
condOpExp :: (Lookup xs "label" UniqueEff, Lookup xs "temp" UniqueEff) => IR.RelOp -> Exp -> Exp -> Eff xs Exp
condOpExp op left right = do
  lefte <- unEx left
  righte <- unEx right
  pure . Cx $ \t f -> IR.CJump op lefte righte t f

stringOpExp :: forall f xs. (
    F.Frame f
  , Lookup xs "label" UniqueEff
  ) => T.LOp' -> Exp -> Exp -> Eff xs Exp
stringOpExp T.Eq (Ex left) (Ex right) = do
  it <- F.externalCall @f "stringEqual" [left, right]
  pure . Cx $ \t f -> IR.CJump IR.Ne it (IR.Const 0) t f
stringOpExp T.NEq (Ex left) (Ex right) = do
  it <- F.externalCall @f "stringEqual" [left, right]
  pure . Cx $ \t f -> IR.CJump IR.Eq it (IR.Const 0) t f
stringOpExp _ _ _ = undefined

ifElseExp :: (Lookup xs "temp" UniqueEff, Lookup xs "label" UniqueEff) => Exp -> Exp -> Exp -> Eff xs Exp
ifElseExp cond (Nx thenStm) (Nx elseStm) = do
  t <- newLabel
  f <- newLabel
  z <- newLabel
  pure . Nx $ IR.seqStm [
      unCx cond t f
    , IR.Label t
    , thenStm
    , IR.Jump (IR.Name z) [z]
    , IR.Label f
    , elseStm
    , IR.Jump (IR.Name z) [z]
    , IR.Label z
    ]
ifElseExp cond (Cx thenExp) (Cx elseExp) = do
  r <- newTemp
  t <- newLabel
  f <- newLabel
  ret0 <- newLabel
  ret1 <- newLabel
  z <- newLabel
  pure . Ex $ IR.ESeq (IR.seqStm [
      unCx cond t f
    , IR.Label t
    , thenExp ret0 ret1
    , IR.Label f
    , elseExp ret0 ret1
    , IR.Label ret0
    , IR.Move (IR.Temp r) (IR.Const 0)
    , IR.Jump (IR.Name z) [z]
    , IR.Label ret1
    , IR.Move (IR.Temp r) (IR.Const 1)
    , IR.Jump (IR.Name z) [z]
    , IR.Label z
    ]) (IR.Temp r)
ifElseExp cond thenExp elseExp = do
  r <- newTemp
  t <- newLabel
  f <- newLabel
  z <- newLabel
  thenExp' <- unEx thenExp
  elseExp' <- unEx elseExp
  pure . Ex $ IR.ESeq (IR.seqStm [
      unCx cond t f
    , IR.Label t
    , IR.Move (IR.Temp r) thenExp'
    , IR.Jump (IR.Name z) [z]
    , IR.Label f
    , IR.Move (IR.Temp r) elseExp'
    , IR.Jump (IR.Name z) [z]
    , IR.Label z
    ]) (IR.Temp r)


ifNoElseExp :: (Lookup xs "label" UniqueEff) => Exp -> Exp -> Eff xs Exp
ifNoElseExp _ (Cx _) = undefined
ifNoElseExp cond exp = do
  t <- newLabel
  z <- newLabel
  thenStm <- unNx exp
  pure . Nx $ IR.seqStm [
      unCx cond t z
    , IR.Label t
    , thenStm
    , IR.Label z
    ]

recordCreationExp :: forall f xs. (Lookup xs "temp" UniqueEff, Lookup xs "label" UniqueEff, F.Frame f) => [Exp] -> Eff xs Exp
recordCreationExp es = do
  r <- newTemp
  s <- recordCreationStm r es
  pure . Ex $ IR.ESeq s (IR.Temp r)
  where
    allocateRecordStm r n = IR.Move (IR.Temp r) <$> F.externalCall @f "malloc" [IR.Const $ n*F.wordSize @f]
    initializeRecordFieldsStm r = fmap $ \(i, Ex e) -> IR.Move (IR.Mem $ IR.BinOp IR.Plus (IR.Temp r) (IR.Const (i*F.wordSize @f))) e
    recordCreationStm r [] = allocateRecordStm r 0
    recordCreationStm r es = do
      s <- allocateRecordStm r (length es)
      pure . IR.Seq s . IR.seqStm . initializeRecordFieldsStm r $ zip [0..] es

arrayCreationExp :: forall f xs. (Lookup xs "temp" UniqueEff, Lookup xs "label" UniqueEff, F.Frame f) => Exp -> Exp -> Eff xs Exp
arrayCreationExp (Ex size) (Ex init) = do
  r <- newTemp
  allocateArrayStm <- IR.Move (IR.Temp r) <$> F.externalCall @f "initArray" [size, init]
  pure . Ex $ IR.ESeq allocateArrayStm (IR.Temp r)
arrayCreationExp _ _ = undefined

whileLoopExp :: (Lookup xs "label" UniqueEff, Lookup xs "breakpoint" BreakPointEff) => Exp -> Exp -> Eff xs Exp
whileLoopExp _ (Cx _) = undefined
whileLoopExp cond exp = do
  test <- newLabel
  body <- newLabel
  bodyStm <- unNx exp
  fetchCurrentBreakPoint >>= \case
    Nothing -> undefined
    Just done -> pure . Nx $ IR.seqStm [
        IR.Label test
      , unCx cond body done
      , IR.Label body
      , bodyStm
      , IR.Jump (IR.Name test) [test]
      , IR.Label done
      ]

forLoopExp :: (
    F.Frame f
  , Lookup xs "nestingLevel" (NestingLevelEff f)
  , Lookup xs "breakpoint" BreakPointEff
  , Lookup xs "label" UniqueEff
  , Lookup xs "temp" UniqueEff
  ) => Access f -> Exp -> Exp -> Exp -> Eff xs Exp
forLoopExp _ _ _ (Cx _) = undefined
forLoopExp access from@(Ex _) (Ex to) exp = do
  index <- unEx =<< valueIdExp access
  indexInit <- unNx =<< assignExp (Ex index) from
  increment <- unNx =<< assignExp (Ex index) (Ex $ IR.BinOp IR.Plus index (IR.Const 1))
  ul <- IR.Temp <$> newTemp
  loop <- newLabel
  body <- newLabel
  bodyStm <- unNx exp
  fetchCurrentBreakPoint >>= \case
    Nothing -> undefined
    Just done -> pure . Nx $ IR.seqStm [
        indexInit
      , IR.Move ul to
      , IR.Label loop
      , IR.CJump IR.Le index ul body done
      , IR.Label body
      , bodyStm
      , increment
      , IR.Jump (IR.Name loop) [loop]
      , IR.Label done
      ]
forLoopExp _ _ _ _ = undefined


breakExp :: (Lookup xs "breakpoint" BreakPointEff) => Eff xs (Maybe Exp)
breakExp = fetchCurrentBreakPoint >>= \case
  Just done -> pure . Just . Nx $ IR.Jump (IR.Name done) [done]
  Nothing -> pure Nothing

funApplyExp :: (Lookup xs "nestingLevel" (NestingLevelEff f), Lookup xs "label" UniqueEff, Lookup xs "temp" UniqueEff, F.Frame f) => Label -> Level f -> [Exp] -> Eff xs Exp
funApplyExp label parent exps = do
  staticLink <- pullInStaticLinksEff parent
  Ex . IR.Call (IR.Name label) . (:) staticLink <$> mapM unEx exps

assignExp :: (Lookup xs "label" UniqueEff, Lookup xs "temp" UniqueEff) => Exp -> Exp -> Eff xs Exp
assignExp (Ex var) exp = Nx . IR.Move var <$> unEx exp
assignExp _ _ = undefined

varInitExp :: (F.Frame f, Lookup xs "label" UniqueEff, Lookup xs "temp" UniqueEff, Lookup xs "nestingLevel" (NestingLevelEff f)) => Access f -> Exp -> Eff xs Exp
varInitExp access e = flip assignExp e =<< valueIdExp access

seqExp :: (Lookup xs "temp" UniqueEff, Lookup xs "label" UniqueEff) => [Exp] -> Eff xs Exp
seqExp es = case List.splitAt (length es - 1) es of
  ([], []) -> pure unitExp
  ([], [e]) -> pure e
  (es, [e]) -> do
    stms <- mapM unNx es
    case e of
      Nx stm -> pure . Nx . IR.seqStm $ stms ++ [stm]
      _ -> Ex . IR.ESeq (IR.seqStm stms) <$> unEx e
  _ -> undefined

funDecExp :: forall f xs. (F.Frame f, Lookup xs "nestingLevel" (NestingLevelEff f), Lookup xs "fragment" (FragmentEff f)) => Exp -> Eff xs ()
funDecExp exp = fetchCurrentLevelEff >>= \case
  TopLevel -> undefined
  level@(Level r) -> do
    let stm = F.viewShift (r ^. #frame) $ addStoreRV exp
    saveProcEntry level stm
  where
    addStoreRV (Ex e) = IR.Move (IR.Temp (F.rv @f)) e
    addStoreRV (Nx s) = s
    addStoreRV (Cx _) = undefined

letExp :: [Exp] -> Exp -> Exp
letExp [] exp = exp
letExp decs (Ex e) = Ex $ IR.ESeq (IR.seqStm $ fmap (\(Nx s) -> s) decs) e
letExp decs (Nx s) = Nx $ IR.seqStm (((\(Nx s) -> s) <$> decs) ++ [s])
letExp decs (Cx genstm) = Cx $ \t f -> IR.seqStm (((\(Nx s) -> s) <$> decs) ++ [genstm t f])
-- data VarEntry f = Var (Access f)

-- type VEnv f = E.Env (VarEntry f)
-- data TranslateError =
--     VariableUndefined Id
--   | VariableNotInScope Id
-- lookupVarAccess :: (
--     Lookup xs "varEnv" (State (VEnv f))
--   , Lookup xs "translateError"(EitherEff (RealLocated TranslateError))) => LId -> Eff xs (Access f)
-- lookupVarAccess (L loc id) =
--    getsEff #varEnv (E.lookup id) >>= \case
--     Nothing -> throwEff #translateError . L loc $ VariableUndefined id
--     Just (Var v) -> pure $ v ^. #access

-- transVar :: (Lookup xs "nestingLevel" (NestingLevelEff f), Lookup xs "varEnv" (State (VEnv f)), Lookup xs "translateError" (EitherEff (RealLocated TranslateError)), F.Frame f) => Access f -> Eff xs Exp
-- transVar a =
--   varExp a >>= \case
--     Just v -> pure v
--     Nothing -> throwEff #translateError . L loc $ VariableNotInScope (unLId lid)
-- transExp :: (Lookup xs "nestingLevel" (NestingLevelEff f), Lookup xs "varEnv" (State (VEnv f)), Lookup xs "translateError"(EitherEff (RealLocated TranslateError)),  F.Frame f) =>  T.LExp -> Eff xs Exp
-- transExp (L _ (T.Var v)) = transVar v
