module Compiler.Backend (Backend (..)) where

import Compiler.Intermediate.Frame (Frame, ProgramFragments)
import Compiler.Intermediate.Unique qualified as U
import Data.Extensible (Lookup)
import RIO
import Data.Extensible.Effect (Eff)

class Backend frame where
  processBackend :: forall xs. (Lookup xs "temp" U.UniqueEff, Lookup xs "label" U.UniqueEff, Frame frame) => ProgramFragments frame -> Eff xs Utf8Builder
