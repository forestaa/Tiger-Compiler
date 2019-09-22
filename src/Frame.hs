module Frame where

import RIO (Int, Bool, String)

import           Data.Proxy
import qualified Unique as U
import qualified IR
import Data.Extensible

class Frame f where
  type Access f = a | a -> f
  newFrame :: Lookup xs "temp" U.UniqueEff => U.Label -> [Bool] -> Eff xs f
  name :: f -> U.Label
  formals :: f -> [Access f]
  allocLocal ::(Lookup xs "temp" U.UniqueEff) => f -> Bool -> Eff xs (f, Access f)
  fp :: Proxy f -> U.Temp
  exp :: Access f -> IR.Exp -> IR.Exp
  wordSize :: Proxy f -> Int


data Frag f where
  Proc :: Frame f => Record '["body" >: IR.Stm, "frame" >: f] -> Frag f
  String :: U.Label -> String -> Frag f
