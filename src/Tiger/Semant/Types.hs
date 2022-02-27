module Tiger.Semant.Types where

import Id
import RIO
import Unique

data Type
  = TUnit
  | TInt
  | TString
  | TNil
  | TRecord {map :: [(Id, Type)], id :: Unique}
  | TArray {range :: Type, id :: Unique}
  | TName LId
  deriving (Show)

instance Eq Type where
  TUnit == TUnit = True
  TInt == TInt = True
  TString == TString = True
  TNil == TNil = True
  r@(TRecord _ _) == r'@(TRecord _ _) = r.id == r'.id
  a@(TArray _ _) == a'@(TArray _ _) = a.id == a'.id
  _ == _ = False

instance Ord Type where
  (TRecord _ _) <= TNil = True
  ty <= ty' = ty == ty'

isComparable :: Type -> Type -> Bool
isComparable leftTy rightTy = leftTy <= rightTy || rightTy <= leftTy
