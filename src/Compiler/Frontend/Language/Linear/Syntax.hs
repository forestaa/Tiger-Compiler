module Compiler.Frontend.Language.Linear.Syntax where

import Compiler.Frontend.Id
import RIO

data Stm
  = CompoundStm Stm Stm
  | AssignStm Id Exp
  | PrintStm [Exp]
  deriving (Show, Eq)

data Exp
  = Id Id
  | Num Int
  | Plus Exp Exp
  | Minus Exp Exp
  | Times Exp Exp
  | Div Exp Exp
  | ESeq Stm Exp
  deriving (Show, Eq)
