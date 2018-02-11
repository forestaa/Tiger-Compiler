module Tiger.Test where

import Tiger.Lexer

test :: IO ()
test = do
  file <- readFile "test/Tiger/samples/merge.tig"
  print $ scanner file