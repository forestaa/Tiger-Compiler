module Compiler.Frontend.Exception where

import Compiler.Exception (compilerExceptionFromException, compilerExceptionToException)
import Control.Exception
import Data.Data (cast)
import RIO
import RIO.Text qualified as T (unpack)

data SomeFrontendException = forall e. (FrontendException e) => SomeFrontendException e

instance Display SomeFrontendException where
  display (SomeFrontendException e) = display e

instance Show SomeFrontendException where
  show = T.unpack . textDisplay

instance Exception SomeFrontendException where
  toException = compilerExceptionToException
  fromException = compilerExceptionFromException

class (Typeable e, Display e) => FrontendException e where
  toFrontendException :: e -> SomeFrontendException
  fromFrontendException :: SomeFrontendException -> Maybe e
  displayFrontendException :: e -> Text

  toFrontendException = SomeFrontendException
  fromFrontendException (SomeFrontendException e) = cast e
  displayFrontendException = textDisplay

instance FrontendException SomeFrontendException where
  toFrontendException = id
  fromFrontendException = Just
  displayFrontendException (SomeFrontendException e) = displayFrontendException e

frontendExceptionToException :: (FrontendException e) => e -> SomeFrontendException
frontendExceptionToException = SomeFrontendException

frontendExceptionFromException :: (FrontendException e) => SomeFrontendException -> Maybe e
frontendExceptionFromException x = do
  SomeFrontendException a <- fromFrontendException x
  cast a
