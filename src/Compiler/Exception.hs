module Compiler.Exception where

import Control.Exception
import Data.Data (cast)
import RIO

data SomeCompilerException = forall e. (Exception e) => SomeCompilerException e

instance Show SomeCompilerException where
  show (SomeCompilerException e) = show e

instance Exception SomeCompilerException

compilerExceptionToException :: (Exception e) => e -> SomeException
compilerExceptionToException = toException . SomeCompilerException

compilerExceptionFromException :: (Exception e) => SomeException -> Maybe e
compilerExceptionFromException x = do
  SomeCompilerException a <- fromException x
  cast a
