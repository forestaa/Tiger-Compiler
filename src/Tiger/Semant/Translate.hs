module Tiger.Semant.Translate where

import RIO
import qualified RIO.List as List
import qualified RIO.List.Partial as Partial
import Data.Extensible

import qualified Frame as F
import qualified IR
import Unique

import Tiger.Semant.Types
import qualified Tiger.LSyntax as T


data Exp = Ex IR.Exp | Nx IR.Stm | Cx (Label -> Label -> IR.Stm)
instance Eq Exp where
  Ex e == Ex e' = e == e'
  Nx s == Nx s' = s == s'
  Cx c == Cx c' = c (Label "true" (Unique 0)) (Label "false" (Unique 1)) == c' (Label "true" (Unique 0)) (Label "false" (Unique 1))
  _ == _ = False
instance Show Exp where
  show (Ex e) = "Ex: " ++ show e
  show (Nx s) = "Nx: " ++ show s
  show (Cx _) = "Cx"
newtype BreakPointStack = BreakPointStack [Label]
type BreakPointEff = State BreakPointStack
runBreakPointEff :: Eff (("breakpoint" >: BreakPointEff) ': xs) a -> Eff xs a
runBreakPointEff = flip (evalStateEff @"breakpoint") (BreakPointStack [])

withBreakPoint :: forall xs a. (Lookup xs "label" UniqueEff, Lookup xs "breakpoint" BreakPointEff) => Eff xs a -> Eff xs a
withBreakPoint body = do
  pushNewBreakPoint
  ret <- body
  popBreakPoint
  pure ret
  where
    pushNewBreakPoint :: Eff xs ()
    pushNewBreakPoint = do
      done <- newLabel
      modifyEff #breakpoint $ \(BreakPointStack breakpoints) -> BreakPointStack (done:breakpoints)
    popBreakPoint :: Eff xs ()
    popBreakPoint = modifyEff #breakpoint $ \(BreakPointStack breakpoints) -> BreakPointStack (Partial.tail breakpoints)
fetchCurrentBreakPoint :: Lookup xs "breakpoint" BreakPointEff => Eff xs (Maybe Label)
fetchCurrentBreakPoint = getEff #breakpoint >>= \case
  BreakPointStack [] -> pure Nothing
  BreakPointStack (breakpoint:_) -> pure $ Just breakpoint

type FragmentEff f = State [F.ProgramFragment f]
runFragmentEff :: Eff (("fragment" >: FragmentEff f) ': xs) a -> Eff xs (a, [F.ProgramFragment f])
runFragmentEff = flip (runStateEff @"fragment") []
saveProcEntry :: (F.Frame f, Lookup xs "fragment" (FragmentEff f), Lookup xs "label" UniqueEff) => Level f -> IR.Stm -> Eff xs ()
saveProcEntry (Level r) stm = modifyEff #fragment . (:) . F.Proc $ #body @= stm <: #frame @= r ^. #frame <: nil
saveStringEntry :: Lookup xs "fragment" (FragmentEff f) => Label -> String -> Eff xs ()
saveStringEntry label s = modifyEff #fragment . (:) $ F.String label s

unEx :: (Lookup xs "label" UniqueEff, Lookup xs "temp" UniqueEff) => Exp -> Eff xs IR.Exp
unEx (Ex e) = pure e
unEx (Cx genstm) = do
  r <- newTemp
  t <- newLabel
  f <- newLabel
  pure $ IR.ESeq (IR.seqStm [
      IR.Move (IR.Temp r) (IR.Const 1)
    , genstm t f
    , IR.Label f
    , IR.Move (IR.Temp r) (IR.Const 0)
    , IR.Label t
    ]) (IR.Temp r)

unNx :: Lookup xs "label" UniqueEff => Exp -> Eff xs IR.Stm
unNx (Ex e) = pure $ IR.Exp e
unNx (Nx s) = pure s
unNx (Cx genstm) = do
  t <- newLabel
  f <- newLabel
  pure $ IR.seqStm [genstm t f, IR.Label t, IR.Label f]

unCx :: Exp -> Label -> Label -> IR.Stm
unCx (Ex (IR.Const 0)) = \_ f -> IR.Jump (IR.Name f) [f]
unCx (Ex (IR.Const _)) = \t _ -> IR.Jump (IR.Name t) [t]
unCx (Ex e) = IR.CJump IR.Ne e (IR.Const 0)
unCx (Nx _) = undefined
unCx (Cx genstm) = genstm

intExp :: Int -> Exp
intExp i = Ex $ IR.Const i

stringExp :: (Lookup xs "label" UniqueEff, Lookup xs "fragment" (FragmentEff f)) => String -> Eff xs Exp
stringExp s = do
  label <- newLabel
  saveStringEntry label s
  pure . Ex $ IR.Name label

nilExp :: Exp
nilExp = Ex $ IR.Const 0

valueIdExp :: (
    F.Frame f
  , Lookup xs "nestingLevel" (NestingLevelEff f)
  ) => Access f -> Eff xs Exp
valueIdExp (Access r) = Ex . F.exp (r ^. #access) <$> pullInStaticLinksEff (r ^. #level)
valueRecFieldExp :: forall f. F.Frame f => Exp -> Int -> Exp
valueRecFieldExp (Ex recordVarExp) fieldNumber = Ex $ IR.Mem (IR.BinOp IR.Plus recordVarExp (IR.Const (fieldNumber * F.wordSize @f)))
valueArrayIndexExp :: forall f. F.Frame f => Exp -> Exp -> Exp
valueArrayIndexExp (Ex arrayVarExp) (Ex indexExp) = Ex $ IR.Mem (IR.BinOp IR.Plus arrayVarExp (IR.BinOp IR.Mul indexExp (IR.Const (F.wordSize @f))))

-- TODO: string comparison
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

    relOpConvert T.Eq    = IR.Eq
    relOpConvert T.NEq   = IR.Ne
    relOpConvert T.Lt    = IR.Lt
    relOpConvert T.Le    = IR.Le
    relOpConvert T.Gt    = IR.Gt
    relOpConvert T.Ge    = IR.Ge

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


ifNoElseExp :: (Lookup xs "temp" UniqueEff, Lookup xs "label" UniqueEff) => Exp -> Exp -> Eff xs Exp
ifNoElseExp cond (Nx thenStm) = do
  r <- newTemp
  t <- newLabel
  z <- newLabel
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

whileLoopExp :: (Lookup xs "label" UniqueEff, Lookup xs "breakpoint" BreakPointEff) => Exp -> Exp -> Eff xs Exp
whileLoopExp cond (Nx bodyStm) = do
  test <- newLabel
  body <- newLabel
  fetchCurrentBreakPoint >>= \case
    -- Nothing -> pure Nothing
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
  , Lookup xs "label" UniqueEff
  , Lookup xs "temp" UniqueEff
  ) => Access f -> Exp -> Exp -> Exp -> Eff xs Exp
forLoopExp access from@(Ex _) (Ex to) (Nx bodyStm) = do
  index <- unEx =<< valueIdExp access
  indexInit <- unNx $ assignExp (Ex index) from
  increment <- unNx $ assignExp (Ex index) (Ex $ IR.BinOp IR.Plus index (IR.Const 1))
  ul <- IR.Temp <$> newTemp
  loop <- newLabel
  body <- newLabel
  done <- newLabel
  pure . Nx $ IR.seqStm [
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


breakExp :: (Lookup xs "breakpoint" BreakPointEff) => Eff xs (Maybe Exp)
breakExp = fetchCurrentBreakPoint >>= \case
  Just done -> pure . Just . Nx $ IR.Jump (IR.Name done) [done]
  Nothing -> pure Nothing

funApplyExp :: (Lookup xs "nestingLevel" (NestingLevelEff f), Lookup xs "label" UniqueEff, Lookup xs "temp" UniqueEff, F.Frame f) => Label -> Level f -> [Exp] -> Eff xs Exp
funApplyExp label level exps = do
  staticLink <- pullInStaticLinksEff level
  Ex . IR.Call (IR.Name label) . (:) staticLink <$> mapM unEx exps

assignExp :: Exp -> Exp -> Exp
assignExp (Ex var) (Ex e) = Nx $ IR.Move var e

varInitExp :: (F.Frame f, Lookup xs "nestingLevel" (NestingLevelEff f)) => Access f -> Exp -> Eff xs Exp
varInitExp access e = flip assignExp e <$> valueIdExp access

seqExp :: (Lookup xs "temp" UniqueEff, Lookup xs "label" UniqueEff) => [Exp] -> Eff xs Exp
seqExp es = case List.uncons es of
  Just (e, es) -> do
    stms <- mapM unNx es
    exp <- unEx e
    pure . Ex $ IR.ESeq (IR.seqStm stms) exp

funDecExp :: forall f xs. (F.Frame f, Lookup xs "nestingLevel" (NestingLevelEff f), Lookup xs "label" UniqueEff, Lookup xs "fragment" (FragmentEff f)) => Exp -> Eff xs ()
funDecExp exp = fetchCurrentLevelEff >>= \case
  level@(Level r) -> do
    let stm = F.viewShift (r ^. #frame) $ addStoreRV exp
    saveProcEntry level stm
  where
    addStoreRV (Ex e) = IR.Move (IR.Temp (F.rv @f)) e
    addStoreRV (Nx s) = s

letExp :: [Exp] -> Exp -> Exp
letExp decs (Ex e) = Ex $ IR.ESeq (IR.seqStm $ fmap (\(Nx s) -> s) decs) e
letExp decs (Nx s) = Nx $ IR.seqStm (((\(Nx s) -> s) <$> decs) ++ [s])
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
