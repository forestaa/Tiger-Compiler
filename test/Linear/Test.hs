module Linear.Test where

import RIO
import System.IO

import qualified Data.ByteString.Lazy as B

import Lexer.Monad

import Linear.LSyntax
import Linear.Lexer
import Linear.Parser
import Linear.Eval

lexerTest :: IO ()
lexerTest = do
  let file = "test/Linear/samples/test.ln"
  bs <- B.readFile file
  print $ scanner file bs

parserTest :: IO ()
parserTest = do
  let file = "test/Linear/samples/test.ln"
  bs <- B.readFile file
  print $ runP parser file bs

evalTest :: IO ()
evalTest = do
  let file = "test/Linear/samples/test.ln"
  bs <- B.readFile file
  case runP parser file bs of
    Right ast -> void $ runInit (unLStm ast)
    Left msg -> print msg