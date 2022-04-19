module Compiler.Frontend where

import Compiler.Exception (compilerExceptionFromException, compilerExceptionToException)
import Compiler.Intermediate.Frame (Frame, ProgramFragment)
import Compiler.Intermediate.Unique qualified as U
import Control.Exception
import Data.ByteString.Lazy qualified as B
import Data.Data (cast)
import Data.Extensible (Lookup)
import Data.Extensible.Effect (Eff, EitherEff)
import RIO

class Frontend language where
  processFrontend :: forall language f xs. (Lookup xs "temp" U.UniqueEff, Lookup xs "label" U.UniqueEff, Lookup xs "exception" (EitherEff SomeFrontendException), Frame f) => FilePath -> B.ByteString -> Eff xs [ProgramFragment f]

data SomeFrontendException = forall e. FrontendException e => SomeFrontendException e

instance Show SomeFrontendException where
  show (SomeFrontendException e) = show e

instance Exception SomeFrontendException where
  toException = compilerExceptionToException
  fromException = compilerExceptionFromException

class (Typeable e, Show e) => FrontendException e where
  toFrontendException :: e -> SomeFrontendException
  fromFrontendException :: SomeFrontendException -> Maybe e
  displayFrontendException :: e -> String

  toFrontendException = SomeFrontendException
  fromFrontendException (SomeFrontendException e) = cast e
  displayFrontendException = show

instance FrontendException SomeFrontendException where
  toFrontendException = id
  fromFrontendException = Just
  displayFrontendException (SomeFrontendException e) = displayFrontendException e

frontendExceptionToException :: FrontendException e => e -> SomeFrontendException
frontendExceptionToException = SomeFrontendException

frontendExceptionFromException :: FrontendException e => SomeFrontendException -> Maybe e
frontendExceptionFromException x = do
  SomeFrontendException a <- fromFrontendException x
  cast a
