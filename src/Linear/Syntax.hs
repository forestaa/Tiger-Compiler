module Linear.Syntax where

import qualified Data.Map as M

import SrcLoc


type Id = String
type Value = Int
type Env = M.Map Id Value

data Stm = CompoundStm Stm Stm
         | AssignStm Id Exp
         | PrintStm [Exp]
         deriving (Show, Eq)
data Exp = Id Id
         | Num Value
         | Plus Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         | Div Exp Exp
         | ESeq Stm Exp
         deriving (Show, Eq)


type LStm = RealLocated LStm'
type LExp = RealLocated LExp'

data LStm' = LCompoundStm LStm LStm
           | LAssignStm Id LExp
           | LPrintStm [LExp]
           deriving (Show, Eq)
data LExp' = LId Id
           | LNum Value
           | LPlus LExp LExp
           | LMinus LExp LExp
           | LTimes LExp LExp
           | LDiv LExp LExp
           | LESeq LStm LExp
           deriving (Show, Eq)

unLStm :: LStm -> Stm
unLStm (L _ (LCompoundStm s1 s2)) = CompoundStm (unLStm s1) (unLStm s2)
unLStm (L _ (LAssignStm id e)) = AssignStm id (unLExp e)
unLStm (L _ (LPrintStm es)) = PrintStm $ map unLExp es

unLExp :: LExp -> Exp
unLExp (L _ (LId id)) = Id id
unLExp (L _ (LNum v)) = Num v
unLExp (L _ (LPlus e1 e2)) = Plus (unLExp e1) (unLExp e2)
unLExp (L _ (LMinus e1 e2)) = Minus (unLExp e1) (unLExp e2)
unLExp (L _ (LTimes e1 e2)) = Times (unLExp e1) (unLExp e2)
unLExp (L _ (LDiv e1 e2)) = Div (unLExp e1) (unLExp e2)
unLExp (L _ (LESeq s e)) = ESeq (unLStm s) (unLExp e)
