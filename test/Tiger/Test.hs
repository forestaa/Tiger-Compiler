module Tiger.Test where

import qualified Data.ByteString.Lazy as B
import Control.Monad.Except

import Lexer.Monad

import Tiger.Syntax
import Tiger.Lexer
import Tiger.Parser


test :: IO ()
test = testAll
-- test = runExceptT (test1 "test/Tiger/samples/merge.tig") >>= print

testAll :: IO ()
testAll = runExceptT (traverse test1 testcases) >>= print

test1 :: FilePath -> ExceptT String IO LExp
test1 file = do
  ExceptT $ scannerTest file
  ExceptT $ parserTest file

scannerTest :: FilePath -> IO (Either String [Lexeme])
scannerTest file = do
  bs <- B.readFile file
  return $ scanner file bs

parserTest :: FilePath -> IO (Either String LExp)
parserTest file = do
  bs <- B.readFile file
  return $ runP parser file bs

testcases :: [FilePath]
testcases = (++) <$> (("test/Tiger/samples/test" ++) <$> cases) <*> [".tig"]
  where
    cases = ((:) '0' . show <$> [1..9]) ++ (show <$> [10..49])

