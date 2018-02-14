{-# LANGUAGE DuplicateRecordFields #-}


module Tiger.Syntax where

-- type Pos = Int
type Symbol = String

data Var = Var Symbol | FieldVar Var Symbol | SubscriptVar Var Exp

data Exp =
    Nil
  | Id Var
  | Int Int
  | String String
  | Call { func :: Symbol, args :: [Exp]}
  | Op {left :: Exp, op :: Op, right :: Exp}
  | Record {fields :: [Field], typ :: Symbol}
  | Seq [Exp]
  | Assign {var :: Var, exp :: Exp}
  | If {bool :: Exp, then' :: Exp, else' :: Exp}
  | While {bool :: Exp, body :: Exp}
  | For {ite :: Symbol, lo :: Exp, hi :: Exp, body :: Exp}
  | Break
  | Let {decs :: [Dec], body :: Exp}

data Op = Plus | Minus | Times | Divide | Eq | NEq | Lt | Le | Gt | Ge
data Field = Field Symbol Exp
data Dec =
   FunDecs [FunDec]
 | VarDec {name :: Symbol, t :: Symbol, init :: Exp}
 | TypeDecs [TypeDec]
data FunDec = FunDec {name :: Symbol, params :: [Field], result :: Symbol}
data TypeDec = TypeDec {name :: Symbol, ty :: Type}
data Type = TName Symbol | TRecord [Field] | TArray Symbol

