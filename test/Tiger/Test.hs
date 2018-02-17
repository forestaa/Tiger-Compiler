module Tiger.Test where

import qualified Data.ByteString.Lazy as B

import Lexer.Monad

import Tiger.Lexer
import Tiger.Parser


test :: IO ()
test = do
  let file = "test/Tiger/samples/test02.tig"
  scanner_test file
  parser_test file

scanner_test :: FilePath -> IO ()
scanner_test file = do
  bs <- B.readFile file
  print $ scanner file bs

parser_test ::FilePath -> IO ()
parser_test file = do
  bs <- B.readFile file
  print $ runP file bs parser


