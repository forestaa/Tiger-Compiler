module X86.Frame where

import Frame qualified
import RIO
import Unique (newStringTemp)
import Unique qualified as U

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
