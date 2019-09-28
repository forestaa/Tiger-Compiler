module Tiger.Semant.Translate where

import RIO
import qualified RIO.List as List
import qualified RIO.List.Partial as List
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

withBreakPoint :: (Lookup xs "label" UniqueEff, Lookup xs "breakpoint" BreakPointEff) => Eff xs a -> Eff xs a
withBreakPoint body = do
  pushNewBreakPoint
  ret <- body
  popBreakPoint
  pure ret
  where
    pushNewBreakPoint :: (Lookup xs "label" UniqueEff, Lookup xs "breakpoint" BreakPointEff) => Eff xs ()
    pushNewBreakPoint = do
      done <- newLabel
      modifyEff #breakpoint $ \(BreakPointStack breakpoints) -> BreakPointStack (done:breakpoints)
    popBreakPoint :: Lookup xs "breakpoint" BreakPointEff => Eff xs ()
    popBreakPoint = modifyEff #breakpoint $ \(BreakPointStack breakpoints) -> BreakPointStack (List.tail breakpoints)
fetchCurrentBreakPoint :: Lookup xs "breakpoint" BreakPointEff => Eff xs (Maybe Label)
fetchCurrentBreakPoint = getEff #breakpoint >>= \case
  BreakPointStack [] -> pure Nothing
  BreakPointStack (breakpoint:_) -> pure $ Just breakpoint


unEx :: (Lookup xs "label" UniqueEff, Lookup xs "temp" UniqueEff) => Exp -> Eff xs IR.Exp
unEx (Ex e) = pure e
unEx (Nx s) = pure $ IR.ESeq s (IR.Const 0)
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
unNx (Cx genstm) = genstm <$> newLabel <*> newLabel

unCx :: Exp -> Label -> Label -> IR.Stm
unCx (Ex e) = undefined
unCx (Nx _) = undefined
unCx (Cx genstm) = genstm

intExp :: Int -> Exp
intExp i = Ex $ IR.Const i

valueIdExp :: (
    F.Frame f
  , Lookup xs "nestingLevel" (NestingLevelEff f)
  ) => Access f -> Eff xs Exp
valueIdExp (Access r) = Ex . F.exp (r ^. #access) <$> pullInStaticLinksEff (r ^. #level)
valueRecFieldExp :: forall f. F.Frame f => Exp -> Int -> Exp
valueRecFieldExp (Ex recordVarExp) fieldNumber = Ex $ IR.Mem (IR.BinOp IR.Minus recordVarExp (IR.Const (fieldNumber * F.wordSize @f)))
valueArrayIndexExp :: forall f. F.Frame f => Exp -> Exp -> Exp
valueArrayIndexExp (Ex arrayVarExp) (Ex indexExp) = Ex $ IR.Mem (IR.BinOp IR.Minus arrayVarExp (IR.BinOp IR.Mul indexExp (IR.Const (F.wordSize @f))))

-- TODO: string comparison
binOpExp :: T.LOp' -> Exp -> Exp -> Exp
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

arithmeticOpExp :: IR.BinOp -> Exp -> Exp -> Exp
arithmeticOpExp op (Ex left) (Ex right) = Ex $ IR.BinOp op left right
condOpExp :: IR.RelOp -> Exp -> Exp -> Exp
condOpExp op (Ex left) (Ex right) = Cx $ \t f -> IR.CJump op left right t f

ifElseExp :: (Lookup xs "temp" UniqueEff, Lookup xs "label" UniqueEff) => Exp -> Exp -> Exp -> Eff xs Exp
ifElseExp cond (Ex thenExp) (Ex elseExp) = do
  r <- newTemp
  t <- newLabel
  f <- newLabel
  z <- newLabel
  pure . Ex $ IR.ESeq (IR.seqStm [
      unCx cond t f
    , IR.Label t
    , IR.Move (IR.Temp r) thenExp
    , IR.Jump (IR.Name z) [z]
    , IR.Label f
    , IR.Move (IR.Temp r) elseExp
    , IR.Jump (IR.Name z) [z]
    , IR.Label z
    ]) (IR.Temp r)
ifElseExp cond (Nx thenStm) (Nx elseStm) = do
  t <- newLabel
  f <- newLabel
  z <- newLabel
  pure . Nx $IR.seqStm [
      unCx cond t f
    , IR.Label t
    , thenStm
    , IR.Jump (IR.Name z) [z]
    , IR.Label f
    , elseStm
    , IR.Jump (IR.Name z) [z]
    , IR.Label z
    ]

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
  s <- allocateRecordStm r (length es)
  pure . Ex $ IR.ESeq (IR.Seq s (IR.seqStm . recordCreationExpInternal r $ zip [0..] es)) (IR.Temp r)
  where
    allocateRecordStm r n = IR.Move (IR.Temp r) <$> F.externalCall @f "malloc" [IR.Const $ n*F.wordSize @f]
    recordCreationExpInternal r = fmap $ \(i, Ex e) -> IR.Move (IR.Mem $ IR.BinOp IR.Plus (IR.Temp r) (IR.Const (i*F.wordSize @f))) e

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

breakExp :: (Lookup xs "breakpoint" BreakPointEff) => Eff xs (Maybe Exp)
breakExp = fetchCurrentBreakPoint >>= \case
  Just label -> pure . Just . Nx $ IR.Jump (IR.Name label) [label]
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
