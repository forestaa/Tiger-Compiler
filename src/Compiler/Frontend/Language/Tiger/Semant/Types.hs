module Compiler.Frontend.Language.Tiger.Semant.Types where

import Compiler.Frontend.Id
import Compiler.Intermediate.Unique
import RIO

data Type
  = TUnit
  | TInt
  | TString
  | TNil
  | TRecord {map :: [(Id, Type)], id :: Unique}
  | TArray {range :: Type, id :: Unique}
  | TName LId
  deriving (Show)

instance Display Type where
  display = displayShow

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
