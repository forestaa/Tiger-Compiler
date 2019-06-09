module Frame where

import RIO (Bool)

import qualified Unique as U

class Frame f where
  type Access f
  newFrame :: U.Label -> [Bool] -> f
  name :: f -> U.Label
  formals :: f -> [Access f]
  allocLocal :: f -> Bool -> Access f
