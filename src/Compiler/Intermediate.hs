module Compiler.Intermediate where

import Compiler.Intermediate.IR qualified as IR
import Compiler.Intermediate.Unique qualified as U
import Data.Extensible (Lookup)
import Data.Extensible.Effect (Eff)

-- NOTE: Bad Interface. For testing.
class Intermediate im where
  processIntermediate :: (Lookup xs "temp" U.UniqueEff, Lookup xs "label" U.UniqueEff) => IR.Stm -> Eff xs [IR.Stm]
