module Compiler.Frontend.Language.Tiger where

import Compiler.Frontend (Frontend (processFrontend))
import Compiler.Frontend.Exception (FrontendException (toFrontendException), SomeFrontendException)
import Compiler.Frontend.Language.Tiger.Parser (parser)
import Compiler.Frontend.Language.Tiger.Semant (translateProgram)
import Compiler.Frontend.Lexer (runP)
import Data.Extensible (Lookup)
import Data.Extensible.Effect (Eff, EitherEff, throwEff)
import RIO

data Tiger = Tiger

instance Frontend Tiger where
  processFrontend file bs = do
    exp <- either throw pure $ runP parser file bs
    either throw pure =<< translateProgram exp
    where
      throw :: (Lookup xs "exception" (EitherEff SomeFrontendException), FrontendException e) => e -> Eff xs a
      throw = (throwEff #exception) . toFrontendException
