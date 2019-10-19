module Tiger.Semant.Types where

import           Data.Extensible
import           RIO

import           Id
import           Unique

data Type = TUnit
          | TInt
          | TString
          | TNil
          | TRecord (Record '["map" :> [(Id, Type)], "id" :> Unique])
          | TArray (Record '["range" :> Type, "id" :> Unique])
          | TName LId
          deriving (Show)
instance Eq Type where
  TUnit == TUnit = True
  TInt == TInt = True
  TString == TString = True
  TNil == TNil = True
  (TRecord r) == (TRecord r') = r ^. #id == r' ^. #id
  (TArray a) == (TArray a') = a ^. #id == a' ^. #id
  _ == _ = False
instance Ord Type where
  (TRecord _) <= TNil = True
  ty <= ty' = ty == ty'
isComparable :: Type -> Type -> Bool
isComparable leftTy rightTy = leftTy <= rightTy || rightTy <= leftTy
