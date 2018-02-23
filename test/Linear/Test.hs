module Linear.Test where

import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict as M
import Control.Monad

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
    Right ast -> void $ run M.empty (unLStm ast)
    Left msg -> print msg