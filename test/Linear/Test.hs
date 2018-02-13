module Linear.Test where

import qualified Data.ByteString.Lazy as B

import Linear.Lexer
import Linear.Parser
import Lexer.Monad


lexer_test :: IO ()
lexer_test = do
  let file = "test/Linear/samples/test.ln"
  bs <- B.readFile file
  print $ scanner file bs

parser_test :: IO ()
parser_test = do
  let file = "test/Linear/samples/test.ln"
  bs <- B.readFile file
  print $ runP file bs parser