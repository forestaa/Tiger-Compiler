module Compiler.Frontend where

import Compiler.Frontend.Exception
import Compiler.Intermediate.Frame (Frame, ProgramFragments)
import Compiler.Intermediate.Unique qualified as U
import Data.Extensible (Lookup)
import Data.Extensible.Effect (Eff, EitherEff)
import RIO

class Frontend language where
  processFrontend :: forall f xs. (Lookup xs "temp" U.UniqueEff, Lookup xs "label" U.UniqueEff, Lookup xs "frontendException" (EitherEff SomeFrontendException), Frame f) => FilePath -> LByteString -> Eff xs (ProgramFragments f)
