{-# LANGUAGE DuplicateRecordFields #-}


module Tiger.Syntax where


import SrcLoc


type Symbol = String

type LVar = RealLocated LVar'
type LExp = RealLocated LExp'
type LField = RealLocated LField'
type LDec = RealLocated LDec'
type LFunDec = RealLocated LFunDec'
type LTypeDec = RealLocated LTypeDec'
type LType = RealLocated LType'

data LVar' = Var Symbol | FieldVar LVar Symbol | SubscriptVar LVar LExp

data LExp' =
    Nil
  | Id LVar
  | Int Int
  | String String
  | Call { func :: Symbol, args :: [LExp]}
  | Op {left :: LExp, op :: Op, right :: LExp}
  | Record {fields :: [LField], typ :: Symbol}
  | Seq [LExp]
  | Assign {var :: LVar, exp :: LExp}
  | If {bool :: LExp, then' :: LExp, else' :: Maybe LExp}
  | While {bool :: LExp, body :: LExp}
  | For {ite :: Symbol, lo :: LExp, hi :: LExp, body :: LExp}
  | Break
  | Let {decs :: [LDec], body :: LExp}

data Op = Plus | Minus | Times | Divide | Eq | NEq | Lt | Le | Gt | Ge
data LField' = Field Symbol LExp
data LDec' =
   FunDecs [LFunDec]
 | VarDec {name :: Symbol, t :: Maybe Symbol, init :: LExp}
 | TypeDecs [LTypeDec]
data LFunDec' = FunDec {name :: Symbol, params :: [LField], result :: Symbol}
data LTypeDec' = TypeDec {name :: Symbol, ty :: LType}
data LType' = TName Symbol | TRecord [LField] | TArray Symbol

