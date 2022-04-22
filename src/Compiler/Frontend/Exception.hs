module Compiler.Frontend.Exception where

import Compiler.Exception (compilerExceptionFromException, compilerExceptionToException)
import Control.Exception
import Data.Data (cast)
import RIO

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
