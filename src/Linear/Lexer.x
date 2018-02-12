{
{-# LANGUAGE StandaloneDeriving #-}

module Linear.Lexer where

import qualified Data.Map as M (Map, empty)
import Data.Maybe
import Control.Monad
import Data.List
import Debug.Trace
}

%wrapper "monadUserState"

$whitespace = [\ \t\b]
$digit = 0-9
$alpha = [A-Za-z]
$special = [\+\-\*\/:\=\(\)\;\,]
$errors = ~[$alpha $digit $whitespace $special]

@number = [$digit]+
@identifier = $alpha($alpha|_|$digit)*


linear:-

<0>  "print" { tokenOf PRINT }
<0>  :\= { tokenOf ASSIGN }
<0>  \+ { tokenOf PLUS }
<0>  \- { tokenOf MINUS }
<0>  \* { tokenOf TIMES }
<0>  \/ { tokenOf DIVIDE }
<0>  \( { tokenOf LPAREN }
<0>  \) { tokenOf RPAREN }
<0>  \; { tokenOf SEMICOLON }
<0>  \, { tokenOf COMMA }
<0>  @number { getNum }
<0>  @identifier { getId }
<0>  $whitespace+ ;
<0>  \n ;
<0>  $errors+ { lexerError }

{

type Letters = Maybe String
showLetters :: Letters -> String
showLetters Nothing = ""
showLetters (Just s) = "string = " ++ show s

data Lexeme = Lexeme AlexPosn LexemeClass Letters
instance Show Lexeme where
  show (Lexeme _ EOF _) = "  Lexeme EOF"
  show (Lexeme p cl mbs) = concat ["  Lexeme class = ", show cl, " posn = ", showPosn p, showLetters mbs]
showPosn :: AlexPosn -> String
showPosn (AlexPn _ row col) = concat ["(row=", show row, ",", "col=", show col, ")"]
data LexemeClass =
    EOF
  | ID String
  | NUM Int
  | COMMA
  | SEMICOLON
  | LPAREN
  | RPAREN
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | PRINT
  deriving (Show, Eq)

type Pos = Maybe AlexPosn
deriving instance Show AlexState
data AlexUserState = AlexUserState
                   {
                     -- used by the parser phase
                       parserCollIdent    :: M.Map String Int
                     , parserCurrentToken :: Lexeme
                     , parserPos          :: Pos
                   } deriving (Show)
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
                   {
                       parserCollIdent    = M.empty
                     , parserCurrentToken = Lexeme undefined EOF Nothing
                     , parserPos          = Nothing
                   }
alexEOF :: Alex Lexeme
alexEOF = return $ Lexeme undefined EOF Nothing


-- lexer main
lexer :: (Lexeme -> Alex a) -> Alex a
lexer = (>>=) alexMonadScan

scanner :: String -> Either String [Lexeme]
scanner str = runAlex str loop
  where
    loop = do
      tok <- alexMonadScan
      case tok of
        Lexeme _ EOF _ -> return [tok]
        Lexeme _ cl  _ -> (:) tok <$> loop

lexerError :: AlexInput -> Int -> Alex a
lexerError (AlexPn _ row col, _, _, input) len = alexError $ concat ["lexical error at line ", show row, ", column ", show col, ": ", take len input]

tokenOf :: LexemeClass -> AlexInput -> Int -> Alex Lexeme
tokenOf c (p, _, _, input) len = return $ Lexeme p c (Just $ take len input)

getNum :: AlexInput -> Int -> Alex Lexeme
getNum (p, _, _, input) len = return $ Lexeme p (NUM $ read str) (Just str)
  where
    str = take len input

getId :: AlexInput -> Int -> Alex Lexeme
getId (p, _, _, input) len = return $ Lexeme p (ID str) (Just str)
  where
    str = take len input





-- maybe useful to report error: alexMonadScan `catch` someerror
catch :: Alex a -> (String -> Alex a) -> Alex a
catch m h = Alex $ \ s -> case unAlex m s of
  Right a -> Right a
  Left a  -> unAlex (h a) s
  -- where
  --   catch' :: Either String a -> (String -> Either String a) -> Either String a
  --   Right a `catch'` _ = Right a
  --   Left a `catch'` h = h a
  --   liftCatch :: (Either String (AlexState, a) -> (String -> Either String (AlexState, a)) -> Either String (AlexState, a)) -> (Alex a -> (String -> Alex a) -> Alex a)
  --   liftCatch catchE m h =
  --       Alex $ \ s -> unAlex m s `catchE` \ message -> unAlex (h message) s
}