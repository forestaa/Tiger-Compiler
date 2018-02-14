module Linear.Test where

import qualified Data.ByteString.Lazy as B

import qualified Data.Map.Strict as M

import Lexer.Monad

import Linear.Lexer
import Linear.Parser
import Linear.Interpreter
import Linear.Syntax

lexer_test :: IO ()
lexer_test = do
  let file = "test/Linear/samples/test.ln"
  bs <- B.readFile file
  print $ scanner file bs

parser_test :: IO ()
parser_test = do
  let file = "test/Linear/samples/test.ln"
  bs <- B.readFile file
  print $ runP file bs parser

interpreter_test :: IO ()
interpreter_test = do
  let file = "test/Linear/samples/test.ln"
  bs <- B.readFile file
  case runP file bs parser of
    Right ast -> run M.empty (unLStm ast) >> return ()
    Left msg -> print msg