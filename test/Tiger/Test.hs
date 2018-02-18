module Tiger.Test where

import qualified Data.ByteString.Lazy as B
-- import Contol.Monad
import Control.Monad.Except

import Lexer.Monad

import Tiger.Syntax
import Tiger.Lexer
import Tiger.Parser


test :: IO ()
-- test = test_all
test = runExceptT (test_1 "test/Tiger/samples/merge.tig") >>= print

test_all :: IO ()
test_all = runExceptT (sequence $ fmap test_1 testcases) >>= print

test_1 :: FilePath -> ExceptT String IO LExp
test_1 file = do
  ExceptT $ scanner_test file
  ExceptT $ parser_test file

scanner_test :: FilePath -> IO (Either String [Lexeme])
scanner_test file = do
  bs <- B.readFile file
  return $ scanner file bs

parser_test :: FilePath -> IO (Either String LExp)
parser_test file = do
  bs <- B.readFile file
  return $ runP parser file bs

testcases :: [FilePath]
testcases = (++) <$> (("test/Tiger/samples/test" ++) <$> cases) <*> [".tig"]
  where
    cases = ((:) '0' . show <$> [1..9]) ++ (show <$> [10..49])

