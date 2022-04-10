module Frame where

import Data.Extensible
import Data.Extensible.Effect
import IR qualified
import RIO
import Unique qualified as U

class Frame f where
  type Access f = a | a -> f
  newFrame :: Lookup xs "temp" U.UniqueEff => U.Label -> [Bool] -> Eff xs f
  name :: f -> U.Label
  formals :: f -> [Access f]
  allocLocal :: (Lookup xs "temp" U.UniqueEff) => f -> Bool -> Eff xs (f, Access f)
  fp :: U.Temp
  rv :: U.Temp
  exp :: Access f -> IR.Exp -> IR.Exp
  wordSize :: Int
  externalCall :: Lookup xs "label" U.UniqueEff => String -> [IR.Exp] -> Eff xs IR.Exp
  procEntryExit1 :: f -> IR.Stm -> IR.Stm

data ProgramFragment f where
  Proc :: Frame f => {body :: IR.Stm, frame :: f} -> ProgramFragment f
  String :: U.Label -> String -> ProgramFragment f

deriving instance Eq f => Eq (ProgramFragment f)

deriving instance Show f => Show (ProgramFragment f)
