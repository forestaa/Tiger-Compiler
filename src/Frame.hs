module Frame where

import RIO (Int, Bool, String)

import qualified Unique as U
import qualified IR
import Data.Extensible

class Frame f where
  type Access f = a | a -> f
  newFrame :: Lookup xs "temp" U.UniqueEff => U.Label -> [Bool] -> Eff xs f
  name :: f -> U.Label
  formals :: f -> [Access f]
  allocLocal ::(Lookup xs "temp" U.UniqueEff) => f -> Bool -> Eff xs (f, Access f)
  fp :: U.Temp
  exp :: Access f -> IR.Exp -> IR.Exp
  wordSize :: Int
  externalCall :: Lookup xs "label" U.UniqueEff => String -> [IR.Exp] -> Eff xs IR.Exp


data ProgramFragment f where
  Proc :: Frame f => Record '["body" >: IR.Stm, "frame" >: f] -> ProgramFragment f
  String :: U.Label -> String -> ProgramFragment f
