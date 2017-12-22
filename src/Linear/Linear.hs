module Linear.Linear where

import qualified Data.Map as M

type Id = String
type Value = Int
type Env = M.Map Id Value

data Stm = CompoundStm Stm Stm
         | AssignStm Id Exp
         | PrintStm [Exp]
data Exp = Id Id | Num Value | BiOp Exp Biop Exp | Eseq Stm Exp
data Biop = Plus | Minus | Times | Div
