module Compiler.Intermediate where

import Compiler.Intermediate.Frame qualified as F
import Compiler.Intermediate.IR qualified as IR
import Compiler.Intermediate.Unique qualified as U
import Data.Extensible (Lookup)
import Data.Extensible.Effect (Eff)

-- NOTE: Bad Interface. For testing.
class Intermediate im where
  processIntermediate :: (F.Frame f, Lookup xs "temp" U.UniqueEff, Lookup xs "label" U.UniqueEff) => F.Procedure f IR.Stm -> Eff xs (F.Procedure f [IR.Stm])
