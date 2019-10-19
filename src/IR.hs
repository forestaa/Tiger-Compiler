module IR where

import RIO

import qualified Unique as U


data Exp = Const Int
         | Name U.Label
         | Temp U.Temp
         | BinOp BinOp Exp Exp
         | Mem Exp
         | Call Exp [Exp]
         | ESeq Stm Exp
  deriving (Eq, Show)

data Stm = Move Exp Exp
         | Exp Exp
         | Jump Exp [U.Label]
         | CJump RelOp Exp Exp U.Label U.Label
         | Seq Stm Stm
         | Label U.Label
  deriving (Eq, Show)

data BinOp = Plus
           | Minus
           | Mul
           | Div
           | And
           | Or
           | LShift
           | RShift
           | ARShift
           | XOr
  deriving (Eq, Show)

data RelOp = Eq
           | Ne
           | Lt
           | Gt
           | Le
           | Ge
           | ULt
           | ULe
           | UGt
           | UGe
  deriving (Eq, Show)

seqStm :: [Stm] -> Stm
seqStm [] = undefined
seqStm [s] = s
seqStm (s:ss) = Seq s $ IR.seqStm ss
