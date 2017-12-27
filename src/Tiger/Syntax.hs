{-# LANGUAGE DuplicateRecordFields #-}


module Tiger.Syntax where

-- type Pos = Int
type Symbol = String

data Var = Var Symbol | FieldVar Var Symbol | SubscriptVar Var Exp

data Exp =
    VarExp Var
  | NilExp
  | IntExp Int
  | StringExp String
  | CallExp { func :: Symbol, args :: [Exp]}
  | OpExp {left :: Exp, op :: Op, right :: Exp}
  | RecordExp {fields :: [Field], typ :: Symbol}
  | SeqExp [Exp]
  | AssignExp {var :: Var, exp :: Exp}
  | IfExp {test :: Exp, then' :: Exp, else' :: Exp}
  | WhileExp {test :: Exp, body :: Exp}
  | ForExp {ite :: Symbol, lo :: Exp, hi :: Exp, body :: Exp}
  | BreakExp
  | LetExp {decs :: [Dec], body :: Exp}

data Op = Plus | Minus | Times | Divide | Eq | NEq | Lt | Le | Gt | Ge
data Field = Field Symbol Exp
data Dec =
   FunDecs [FunDec]
 | VarDec {name :: Symbol, typ :: Symbol, init :: Exp}
 | TypeDec [TyDec]
data FunDec = FunDec {name :: Symbol, params :: [Field], result :: Symbol}
data TyDec = TyDec {name :: Symbol, ty :: Ty}
data Ty = Name Symbol | Record [Field] | Array Symbol

