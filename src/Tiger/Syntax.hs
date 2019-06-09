{-# LANGUAGE DuplicateRecordFields #-}

module Tiger.Syntax where

import RIO

import Id

data Exp =
  -- literals
    Nil
  | Int Int
  | String String

  -- array and record creation
  | ArrayCreate  {typeid :: Id, size :: Exp, init :: Exp}
  | RecordCreate {typeid :: Id, fields :: [FieldAssign]}

  -- variables, field, elements of an array
  | Var Value

  -- function application
  | FunApply { func :: Id, args :: [Exp]}

  -- operations
  | Op {left :: Exp, op :: Op, right :: Exp}
  | Seq [Exp]

  -- assignment
  | Assign {var :: Value, exp :: Exp}

  -- control structure
  | If {bool :: Exp, then' :: Exp, else' :: Maybe Exp}
  | While {bool :: Exp, body :: Exp}
  | For {id :: Id, escape :: Bool, from :: Exp, to :: Exp, body :: Exp}
  | Break
  | Let {decs :: [Dec], body :: Exp}
  deriving (Show, Eq)
data Value = Id Id | RecField Value Id | ArrayIndex Value Exp deriving (Show, Eq)
data FieldAssign = FieldAssign Id Exp deriving (Show, Eq)
data Op = Plus | Minus | Times | Div | Eq | NEq | Lt | Le | Gt | Ge deriving (Show, Eq)

data Dec =
   FunDec {id :: Id, args :: [Field], rettype :: Maybe Id, body :: Exp}
 | VarDec {id :: Id, escape :: Bool, t :: Maybe Id, init :: Exp}
 | TypeDec {id :: Id, ty :: Type}
 deriving (Show, Eq)
data Type = TypeId Id | RecordType [Field] | ArrayType Id deriving (Show, Eq)
data Field = Field {id :: Id, escape :: Bool, typeid :: Id} deriving (Show, Eq)
