module Backend.X86.Frame where

import Intermediate.Frame qualified as Frame
import Intermediate.Unique (newStringTemp)
import Intermediate.Unique qualified as U
import RIO

data Frame = Frame {name :: String, formals :: [Bool]}

data Access = Access

fp :: U.Temp
fp = newStringTemp "fp"

rv :: U.Temp
rv = newStringTemp "RAX"

wordSize :: Int
wordSize = 8

instance Frame.Frame Frame where
  type Access Frame = Access
  fp = fp
  rv = rv
  wordSize = wordSize
