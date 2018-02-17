{-# LANGUAGE DuplicateRecordFields #-}


module Tiger.Syntax where


import SrcLoc


type Id = String

type LExp = RealLocated LExp'
type LValue = RealLocated LValue'
type LFieldAssign = RealLocated LFieldAssign'
type LDec = RealLocated LDec'
type LType = RealLocated LType'
type LField = RealLocated LField'


data LExp' =
  -- literals
    Nil
  | Int Int
  | String String

  -- array and record creation
  | ArrayCreate  {typeid :: Id, size :: LExp, init :: LExp}
  | RecordCreate {typeid :: Id, fields :: [LFieldAssign]}

  -- variables, field, elements of an array
  | Var LValue

  -- function application
  | FunApply { func :: Id, args :: [LExp]}

  -- operations
  | Op {left :: LExp, op :: Op, right :: LExp}
  | Seq [LExp]

  -- assignment
  | Assign {var :: LValue, exp :: LExp}

  -- control structure
  | If {bool :: LExp, then' :: LExp, else' :: Maybe LExp}
  | While {bool :: LExp, body :: LExp}
  | For {id :: Id, from :: LExp, to :: LExp, body :: LExp}
  | Break
  | Let {decs :: [LDec], bodys :: [LExp]}
  deriving (Show, Eq)

data LValue' = Id Id | RecField LValue Id | ArrayIndex LValue LExp deriving (Show, Eq)

data LFieldAssign' = FieldAssign Id LExp deriving (Show, Eq)
data Op = Plus | Minus | Times | Div | Eq | NEq | Lt | Le | Gt | Ge deriving (Show, Eq)

data LDec' =
   FunDec {id :: Id, args :: [LField], rettype :: Maybe Id, body :: LExp}
 | VarDec {id :: Id, t :: Maybe Id, init :: LExp}
 | TypeDec {id :: Id, ty :: LType}
 deriving (Show, Eq)
data LType' = TypeId Id | RecordType [LField] | ArrayType Id deriving (Show, Eq)
data LField' = Field {id :: Id, typeid :: Id} deriving (Show, Eq)
