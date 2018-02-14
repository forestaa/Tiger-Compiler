module Linear.Syntax where

import qualified Data.Map as M

import SrcLoc


type Id = String
type Value = Int
type Env = M.Map Id Value

type LStm = RealLocated LStm'
type LExp = RealLocated LExp'

data LStm' = CompoundStm LStm LStm
           | AssignStm Id LExp
           | PrintStm [LExp]
           deriving (Show, Eq)
data LExp' = Id Id
           | Num Value
           | Plus LExp LExp
           | Minus LExp LExp
           | Times LExp LExp
           | Div LExp LExp
           | ESeq LStm LExp
           deriving (Show, Eq)
