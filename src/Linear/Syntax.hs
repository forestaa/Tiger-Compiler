module Linear.Syntax where

import RIO
-- import qualified RIO.Map as Map
-- import qualified Data.Map as M

import Env
-- import SrcLoc

data Stm = CompoundStm Stm Stm
         | AssignStm Id Exp
         | PrintStm [Exp]
         deriving (Show, Eq)
data Exp = Id Id
         | Num Int
         | Plus Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         | Div Exp Exp
         | ESeq Stm Exp
         deriving (Show, Eq)
