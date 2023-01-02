module Compiler.Frontend.Language.Tiger.Semant.Translate where

import Compiler.Frontend.Id
import Compiler.Frontend.Language.Tiger.LSyntax qualified as T
import Compiler.Frontend.Language.Tiger.Semant.BreakPoint
import Compiler.Frontend.Language.Tiger.Semant.Env
import Compiler.Frontend.Language.Tiger.Semant.Exp
import Compiler.Frontend.Language.Tiger.Semant.Level
import Compiler.Frontend.Language.Tiger.Semant.TypeCheck qualified as TC
import Compiler.Frontend.Language.Tiger.Semant.Types
import Compiler.Frontend.SrcLoc
import Compiler.Intermediate.Frame qualified as F
import Compiler.Intermediate.IR qualified as IR
import Compiler.Intermediate.Unique (UniqueEff, namedLabel, newLabel)
import Data.Extensible
import Data.Extensible.Effect
import RIO
import RIO.List qualified as List

data TranslateError
  = VariableUndefined Id
  | BreakOutsideLoop

instance Show TranslateError where
  show (VariableUndefined id) = "undefined variable: " ++ show id
  show BreakOutsideLoop = "break should be inside while or for loop"

lookupVarAccessEff ::
  ( Lookup xs "varAccessEnv" (State (VAEnv f)),
    Lookup xs "translateError" (EitherEff (RealLocated TranslateError))
  ) =>
  LId ->
  Eff xs (VarAccess f)
lookupVarAccessEff (L loc id) =
  lookupVarAccess id >>= \case
    Just access -> pure access
    Nothing -> throwEff #translateError . L loc $ VariableUndefined id

allocateLocalVariable ::
  ( F.Frame f,
    Lookup xs "varAccessEnv" (State (VAEnv f)),
    Lookup xs "nestingLevel" (NestingLevelEff f),
    Lookup xs "temp" UniqueEff
  ) =>
  Id ->
  Bool ->
  Eff xs (Access f)
allocateLocalVariable id escape = do
  a <- allocateLocalOnCurrentLevel escape
  level <- fetchCurrentLevelEff
  let access = Access level a
  insertVarAccess id $ VarAccess access
  pure access

insertFunAccess ::
  ( Lookup xs "varAccessEnv" (State (VAEnv f)),
    Lookup xs "nestingLevel" (NestingLevelEff f),
    Lookup xs "label" UniqueEff
  ) =>
  Id ->
  Eff xs ()
insertFunAccess name = do
  label <- namedLabel name
  parent <- fetchCurrentLevelEff
  insertVarAccess name $ FunAccess label parent

intExp :: Int -> Exp
intExp i = Ex $ IR.Const i

-- TODO: use Frame.string
stringExp :: (Lookup xs "label" UniqueEff, Lookup xs "fragment" (F.ProgramEff f)) => String -> Eff xs Exp
stringExp s = do
  label <- newLabel
  F.saveStringFragmentEff label s
  pure . Ex $ IR.Name label

nilExp :: Exp
nilExp = Ex $ IR.Const 0

unitExp :: Exp
unitExp = Nx $ IR.Exp (IR.Const 0)

valueIdExp ::
  ( F.Frame f,
    Lookup xs "nestingLevel" (NestingLevelEff f)
  ) =>
  Access f ->
  Eff xs Exp
valueIdExp Access {..} = Ex . F.exp access <$> pullInStaticLinksEff level

valueIdExpEff ::
  ( F.Frame f,
    Lookup xs "varAccessEnv" (State (VAEnv f)),
    Lookup xs "nestingLevel" (NestingLevelEff f),
    Lookup xs "translateError" (EitherEff (RealLocated TranslateError))
  ) =>
  LId ->
  Eff xs Exp
valueIdExpEff lid =
  lookupVarAccessEff lid >>= \case
    VarAccess a -> valueIdExp a
    _ -> undefined

valueRecFieldExp :: forall f. F.Frame f => Exp -> Int -> Exp
valueRecFieldExp (Ex recordVarExp) fieldNumber = Ex $ IR.Mem (IR.BinOp IR.Plus recordVarExp (IR.Const (fieldNumber * F.wordSize @f)))
valueRecFieldExp _ _ = undefined

valueRecFieldExpEff ::
  forall f xs.
  ( F.Frame f,
    Lookup xs "typeEnv" (State TEnv),
    Lookup xs "typeCheckError" (EitherEff (RealLocated TC.TypeCheckError))
  ) =>
  Type ->
  Exp ->
  Id ->
  Eff xs Exp
valueRecFieldExpEff ty varExp field =
  TC.skipName ty >>= \case
    r@TRecord {} -> case List.findIndex (\(id, _) -> id == field) r.map of
      Just i -> pure $ valueRecFieldExp @f varExp i
      _ -> undefined
    _ -> undefined

valueArrayIndexExp :: forall f. F.Frame f => Exp -> Exp -> Exp
valueArrayIndexExp (Ex arrayVarExp) (Ex indexExp) = Ex $ IR.Mem (IR.BinOp IR.Plus arrayVarExp (IR.BinOp IR.Mul indexExp (IR.Const (F.wordSize @f))))
valueArrayIndexExp _ _ = undefined

binOpExp ::
  forall f xs.
  ( F.Frame f,
    Lookup xs "label" UniqueEff,
    Lookup xs "temp" UniqueEff
  ) =>
  Type ->
  T.LOp' ->
  Exp ->
  Exp ->
  Eff xs Exp
binOpExp TString op left right = stringOpExp @f op left right
binOpExp _ op left right = intOpExp op left right

intOpExp :: (Lookup xs "label" UniqueEff, Lookup xs "temp" UniqueEff) => T.LOp' -> Exp -> Exp -> Eff xs Exp
intOpExp op left right
  | isArithmetic op = arithmeticOpExp (arithmeticOpConvert op) left right
  | otherwise = condOpExp (relOpConvert op) left right
  where
    isArithmetic T.Plus = True
    isArithmetic T.Minus = True
    isArithmetic T.Times = True
    isArithmetic T.Div = True
    isArithmetic _ = False

    arithmeticOpConvert T.Plus = IR.Plus
    arithmeticOpConvert T.Minus = IR.Minus
    arithmeticOpConvert T.Times = IR.Mul
    arithmeticOpConvert T.Div = IR.Div
    arithmeticOpConvert _ = undefined

    relOpConvert T.Eq = IR.Eq
    relOpConvert T.NEq = IR.Ne
    relOpConvert T.Lt = IR.Lt
    relOpConvert T.Le = IR.Le
    relOpConvert T.Gt = IR.Gt
    relOpConvert T.Ge = IR.Ge
    relOpConvert _ = undefined

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

stringOpExp ::
  forall f xs.
  ( F.Frame f,
    Lookup xs "label" UniqueEff
  ) =>
  T.LOp' ->
  Exp ->
  Exp ->
  Eff xs Exp
stringOpExp T.Eq (Ex left) (Ex right) = do
  it <- F.externalCall @f "stringEqual" [left, right]
  pure . Cx $ \t f -> IR.CJump IR.Ne it (IR.Const 0) t f
stringOpExp T.NEq (Ex left) (Ex right) = do
  it <- F.externalCall @f "stringEqual" [left, right]
  pure . Cx $ \t f -> IR.CJump IR.Eq it (IR.Const 0) t f
stringOpExp _ _ _ = undefined

ifElseExp ::
  ( F.Frame f,
    Lookup xs "temp" UniqueEff,
    Lookup xs "label" UniqueEff,
    Lookup xs "nestingLevel" (NestingLevelEff f)
  ) =>
  Exp ->
  Exp ->
  Exp ->
  Eff xs Exp
ifElseExp cond (Nx thenStm) (Nx elseStm) = do
  t <- newLabel
  f <- newLabel
  z <- newLabel
  pure . Nx $
    IR.seqStm
      [ unCx cond t f,
        IR.Label t,
        thenStm,
        IR.Jump (IR.Name z) [z],
        IR.Label f,
        elseStm,
        IR.Jump (IR.Name z) [z],
        IR.Label z
      ]
ifElseExp cond (Cx thenExp) (Cx elseExp) = do
  temp <- allocateTempOnCurrentLevel
  t <- newLabel
  f <- newLabel
  ret0 <- newLabel
  ret1 <- newLabel
  z <- newLabel
  pure . Ex $
    IR.ESeq
      ( IR.seqStm
          [ unCx cond t f,
            IR.Label t,
            thenExp ret0 ret1,
            IR.Label f,
            elseExp ret0 ret1,
            IR.Label ret0,
            IR.Move temp (IR.Const 0),
            IR.Jump (IR.Name z) [z],
            IR.Label ret1,
            IR.Move temp (IR.Const 1),
            IR.Jump (IR.Name z) [z],
            IR.Label z
          ]
      )
      temp
ifElseExp cond thenExp elseExp = do
  temp <- allocateTempOnCurrentLevel
  t <- newLabel
  f <- newLabel
  z <- newLabel
  thenExp' <- unEx thenExp
  elseExp' <- unEx elseExp
  pure . Ex $
    IR.ESeq
      ( IR.seqStm
          [ unCx cond t f,
            IR.Label t,
            IR.Move temp thenExp',
            IR.Jump (IR.Name z) [z],
            IR.Label f,
            IR.Move temp elseExp',
            IR.Jump (IR.Name z) [z],
            IR.Label z
          ]
      )
      temp

ifNoElseExp :: (Lookup xs "label" UniqueEff) => Exp -> Exp -> Eff xs Exp
ifNoElseExp _ (Cx _) = undefined
ifNoElseExp cond exp = do
  t <- newLabel
  z <- newLabel
  thenStm <- unNx exp
  pure . Nx $
    IR.seqStm
      [ unCx cond t z,
        IR.Label t,
        thenStm,
        IR.Label z
      ]

recordCreationExp :: forall f xs. (F.Frame f, Lookup xs "temp" UniqueEff, Lookup xs "label" UniqueEff, Lookup xs "nestingLevel" (NestingLevelEff f)) => [Exp] -> Eff xs Exp
recordCreationExp es = do
  temp <- allocateTempOnCurrentLevel
  s <- recordCreationStm temp es
  pure . Ex $ IR.ESeq s temp
  where
    allocateRecordStm temp n = IR.Move temp <$> F.externalCall @f "malloc" [IR.Const $ n * F.wordSize @f]
    initializeRecordFieldsStm temp = fmap $ \(i, Ex e) -> IR.Move (IR.Mem $ IR.BinOp IR.Plus temp (IR.Const (i * F.wordSize @f))) e
    recordCreationStm r [] = allocateRecordStm r 0
    recordCreationStm temp es = do
      s <- allocateRecordStm temp (length es)
      pure . IR.Seq s . IR.seqStm . initializeRecordFieldsStm temp $ zip [0 ..] es

arrayCreationExp :: forall f xs. (F.Frame f, Lookup xs "temp" UniqueEff, Lookup xs "label" UniqueEff, Lookup xs "nestingLevel" (NestingLevelEff f)) => Exp -> Exp -> Eff xs Exp
arrayCreationExp (Ex size) (Ex init) = do
  temp <- allocateTempOnCurrentLevel
  allocateArrayStm <- IR.Move temp <$> F.externalCall @f "initArray" [size, init]
  pure . Ex $ IR.ESeq allocateArrayStm temp
arrayCreationExp _ _ = undefined

whileLoopExp :: (Lookup xs "label" UniqueEff, Lookup xs "breakpoint" BreakPointEff) => Exp -> Exp -> Eff xs Exp
whileLoopExp _ (Cx _) = undefined
whileLoopExp cond exp = do
  test <- newLabel
  body <- newLabel
  bodyStm <- unNx exp
  fetchCurrentBreakPoint >>= \case
    Nothing -> undefined
    Just done ->
      pure . Nx $
        IR.seqStm
          [ IR.Label test,
            unCx cond body done,
            IR.Label body,
            bodyStm,
            IR.Jump (IR.Name test) [test],
            IR.Label done
          ]

forLoopExp ::
  ( F.Frame f,
    Lookup xs "nestingLevel" (NestingLevelEff f),
    Lookup xs "breakpoint" BreakPointEff,
    Lookup xs "label" UniqueEff,
    Lookup xs "temp" UniqueEff
  ) =>
  Access f ->
  Exp ->
  Exp ->
  Exp ->
  Eff xs Exp
forLoopExp _ _ _ (Cx _) = undefined
forLoopExp access from@(Ex _) (Ex to) exp = do
  index <- unEx =<< valueIdExp access
  indexInit <- unNx =<< assignExp (Ex index) from
  increment <- unNx =<< assignExp (Ex index) (Ex $ IR.BinOp IR.Plus index (IR.Const 1))
  ul <- allocateTempOnCurrentLevel
  loop <- newLabel
  body <- newLabel
  bodyStm <- unNx exp
  fetchCurrentBreakPoint >>= \case
    Nothing -> undefined
    Just done ->
      pure . Nx $
        IR.seqStm
          [ indexInit,
            IR.Move ul to,
            IR.Label loop,
            IR.CJump IR.Le index ul body done,
            IR.Label body,
            bodyStm,
            increment,
            IR.Jump (IR.Name loop) [loop],
            IR.Label done
          ]
forLoopExp _ _ _ _ = undefined

breakExp ::
  ( Lookup xs "breakpoint" BreakPointEff,
    Lookup xs "translateError" (EitherEff (RealLocated TranslateError))
  ) =>
  RealSrcSpan ->
  Eff xs Exp
breakExp loc =
  fetchCurrentBreakPoint >>= \case
    Just done -> pure . Nx $ IR.Jump (IR.Name done) [done]
    Nothing -> throwEff #translateError $ L loc BreakOutsideLoop

funApplyExp ::
  ( F.Frame f,
    Lookup xs "varAccessEnv" (State (VAEnv f)),
    Lookup xs "nestingLevel" (NestingLevelEff f),
    Lookup xs "label" UniqueEff,
    Lookup xs "temp" UniqueEff,
    Lookup xs "translateError" (EitherEff (RealLocated TranslateError))
  ) =>
  LId ->
  [Exp] ->
  Eff xs Exp
funApplyExp func exps =
  lookupVarAccessEff func >>= \case
    FunAccess {..} -> do
      staticLink <- pullInStaticLinksEff parent
      Ex . IR.Call (IR.Name label) . (:) staticLink <$> mapM unEx exps
    VarAccess _ -> undefined

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

funDecExp :: forall f xs. (F.Frame f, Lookup xs "nestingLevel" (NestingLevelEff f), Lookup xs "fragment" (F.ProgramEff f)) => Exp -> Eff xs ()
funDecExp exp =
  fetchCurrentLevelEff >>= \case
    TopLevel -> undefined
    level@Level {} -> do
      let stm = F.procEntryExit1 level.frame $ addStoreRV exp
      F.saveFragmentEff level.frame stm
  where
    addStoreRV (Ex e) = IR.Move (IR.Temp (F.rv @f)) e
    addStoreRV (Nx s) = s
    addStoreRV (Cx _) = undefined

letExp :: [Exp] -> Exp -> Exp
letExp [] exp = exp
letExp decs (Ex e) = Ex $ IR.ESeq (IR.seqStm $ fmap (\(Nx s) -> s) decs) e
letExp decs (Nx s) = Nx $ IR.seqStm (((\(Nx s) -> s) <$> decs) ++ [s])
letExp decs (Cx genstm) = Cx $ \t f -> IR.seqStm (((\(Nx s) -> s) <$> decs) ++ [genstm t f])
