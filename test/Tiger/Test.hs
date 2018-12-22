module Tiger.Test where

import RIO
import System.IO

import qualified Data.ByteString.Lazy as B
import Control.Monad.Except

import Lexer.Monad

import Tiger.LSyntax
import Tiger.Lexer
import Tiger.Parser
import Tiger.Typing


test :: IO ()
test = testAll
-- test = runExceptT (test1 "test/Tiger/samples/merge.tig") >>= print

testAll :: IO ()
testAll = runExceptT (traverse test1 testcases) >>= print

test1 :: FilePath -> ExceptT String IO ()
test1 file = do
  -- scannerTest file
  e <- parserTest file
  ty <- liftEither $ typingTest e
  liftIO . putStrLn $ concat ["file: ", show file, ", type = ", show ty]
    


scannerTest :: FilePath -> ExceptT String IO [Lexeme]
scannerTest file = do
  bs <- liftIO $ B.readFile file
  liftEither $ scanner file bs

-- parserTest :: FilePath -> IO (Either String LExp)
parserTest :: FilePath -> ExceptT String IO LExp
parserTest file = do
  bs <- liftIO $ B.readFile file
  liftEither $ runP parser file bs

typingTest :: LExp -> Either String Type
typingTest e = mapLeft show . runTyping $ typingExp e

testcases :: [FilePath]
testcases = (++) <$> (("test/Tiger/samples/test" ++) <$> cases) <*> [".tig"]
  where
    cases = ((:) '0' . show <$> [1..9]) ++ (show <$> [10..49])

