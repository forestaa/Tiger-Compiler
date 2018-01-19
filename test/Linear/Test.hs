module Linear.Test where

import Linear.Lexer

test :: IO ()
test = do
  file <- readFile "test/Linear/test.ln"
  print $ scanner file