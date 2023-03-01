module Compiler.Frontend.Language.Tiger.Semant.Exp where

import Compiler.Frontend.Language.Tiger.Semant.Level (NestingLevelEff, allocateTempOnCurrentLevel)
import Compiler.Intermediate.Frame qualified as F
import Compiler.Intermediate.IR qualified as IR
import Compiler.Intermediate.Unique
import Data.Extensible
import Data.Extensible.Effect
import RIO
import RIO.Text qualified as T (unpack)

data Exp = Ex IR.Exp | Nx IR.Stm | Cx (Label -> Label -> IR.Stm)

instance Eq Exp where
  Ex e == Ex e' = e == e'
  Nx s == Nx s' = s == s'
  Cx c == Cx c' = c true false == c' true false
    where
      (true, false) = leaveEff . evalUniqueEff @"label" $ (,) <$> newLabel <*> newLabel
  _ == _ = False

instance Display Exp where
  display (Ex e) = "Ex: " <> display e
  display (Nx s) = "Nx: " <> display s
  display (Cx genstm) = "Cx: λt. λf -> " <> display (genstm true false)
    where
      (true, false) = leaveEff . evalUniqueEff @"label" $ (,) <$> namedLabel "t" <*> namedLabel "f"

instance Show Exp where
  show = T.unpack . textDisplay

unEx :: (Lookup xs "label" UniqueEff, Lookup xs "temp" UniqueEff, F.Frame f, Lookup xs "nestingLevel" (NestingLevelEff f)) => Exp -> Eff xs IR.Exp
unEx (Ex e) = pure e
unEx (Nx _) = pure $ IR.Const 0
unEx (Cx genstm) = do
  r <- allocateTempOnCurrentLevel
  t <- newLabel
  f <- newLabel
  pure $
    IR.ESeq
      ( IR.seqStm
          [ IR.Move r (IR.Const 1),
            genstm t f,
            IR.Label f,
            IR.Move r (IR.Const 0),
            IR.Label t
          ]
      )
      r

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
