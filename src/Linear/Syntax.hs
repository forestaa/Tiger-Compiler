module Linear.Syntax where

import qualified Data.Map as M

import SrcLoc


type Id = String
type Env = M.Map Id Int


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
