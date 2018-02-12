module Linear.Test where

import Linear.Lexer
import Linear.Parser

lexer_test :: IO ()
lexer_test = do
  file <- readFile "test/Linear/samples/test.ln"
  print $ scanner file

parser_test :: IO ()
parser_test = do
  file <- readFile "test/Linear/samples/test.ln"
  print $ runAlex file parser