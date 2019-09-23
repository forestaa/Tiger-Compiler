module Tiger.Semant.Translate where

import RIO
import Data.Extensible

import qualified Frame as F
import qualified IR
import qualified Unique as U

import Tiger.Semant.Types
import qualified Tiger.LSyntax as T


data Exp = Ex IR.Exp | Nx IR.Stm | Cx (U.Label -> U.Label -> IR.Stm)
instance Eq Exp where
  Ex e == Ex e' = e == e'
  Nx s == Nx s' = s == s'
  Cx c == Cx c' = c (U.Label "true" (U.Unique 0)) (U.Label "false" (U.Unique 1)) == c' (U.Label "true" (U.Unique 0)) (U.Label "false" (U.Unique 1))
  _ == _ = False
instance Show Exp where
  show (Ex e) = "Ex: " ++ show e
  show (Nx s) = "Nx: " ++ show s
  show (Cx _) = "Cx"

unEx :: (Lookup xs "label" U.UniqueEff, Lookup xs "temp" U.UniqueEff) => Exp -> Eff xs IR.Exp
unEx (Ex e) = pure e
unEx (Nx s) = pure $ IR.ESeq s (IR.Const 0)
unEx (Cx genstm) = do
  r <- U.newTemp
  t <- U.newLabel
  f <- U.newLabel
  pure $ IR.ESeq (IR.seqStm [IR.Move (IR.Temp r) (IR.Const 1), genstm t f, IR.Label f, IR.Move (IR.Temp r) (IR.Const 0), IR.Label t]) (IR.Temp r)

unNx :: Lookup xs "label" U.UniqueEff => Exp -> Eff xs IR.Stm
unNx (Ex e) = pure $ IR.Exp e
unNx (Nx s) = pure s
unNx (Cx genstm) = genstm <$> U.newLabel <*> U.newLabel

unCx :: Exp -> U.Label -> U.Label -> IR.Stm
unCx (Ex e) = undefined
unCx (Nx _) = undefined
unCx (Cx genstm) = genstm

valueIdExp :: (
    F.Frame f
  , Lookup xs "nestingLevel" (NestingLevelEff f)
  ) => Access f -> Eff xs (Maybe Exp)
valueIdExp (Access r) = fmap (Ex . F.exp (r ^. #access)) <$> pullInStaticLinksEff (r ^. #level)
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
