module IR where

import RIO hiding (Const)
import Unique qualified as U

data Exp
  = Const Int
  | Name U.Label
  | Temp U.Temp
  | BinOp BinOp Exp Exp
  | Mem Exp
  | Call Exp [Exp]
  | ESeq Stm Exp
  deriving (Eq, Show)

data Stm
  = Move Exp Exp
  | Exp Exp
  | Jump Exp [U.Label]
  | CJump RelOp Exp Exp U.Label U.Label
  | Seq Stm Stm
  | Label U.Label
  deriving (Eq, Show)

data BinOp
  = Plus
  | Minus
  | Mul
  | Div
  deriving (Eq, Show)

data RelOp
  = Eq
  | Ne
  | Lt
  | Gt
  | Le
  | Ge
  deriving (Eq, Show)

seqStm :: [Stm] -> Stm
seqStm [] = noop
seqStm [s] = s
seqStm (s : ss) = Seq s $ IR.seqStm ss

noop :: Stm
noop = Exp (Const 0)

notRelOp :: RelOp -> RelOp
notRelOp Eq = Ne
notRelOp Ne = Eq
notRelOp Lt = Ge
notRelOp Gt = Le
notRelOp Le = Gt
notRelOp Ge = Lt
