module Tiger.Test where

import qualified Data.ByteString.Lazy as B

import Lexer.Monad

import Tiger.Lexer

scanner_test :: IO ()
scanner_test = do
  let file = "test/Tiger/samples/merge.tig"
  bs <- B.readFile file
  print $ scanner file bs
