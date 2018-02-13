{
{-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TypeSynonimInstances #-}
module Linear.Lexer where


import Data.Maybe
import Control.Monad
import Data.List
import Debug.Trace
import qualified Data.ByteString.Lazy.Char8 as B
-- import qualified Data.ByteString.Char8 as B

import SrcLoc
import Lexer.Monad
}

-- %wrapper "monadUserState"

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
-- <0>  $errors+ { lexerError }

{

-- type Letters = Maybe String
-- instance Show Letters where
-- show Nothing = ""
-- show (Just s) = "string = " ++ show s

type Lexeme = RealLocated Token
-- instance Show Lexeme where
--   show (Lexeme _ EOF _) = "  Lexeme EOF"
--   show (Lexeme p cl mbs) = concat ["  Lexeme class = ", show cl, " posn = ", show p, showLetters mbs]

data Token =
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


-- lexer main
lexer :: (Lexeme -> P a) -> P a
lexer = (>>=) lexToken

scanner :: FilePath -> B.ByteString -> Either String [Lexeme]
scanner file buffer = runP file buffer loop
  where
    loop :: P [Lexeme]
    loop = do
      tk <- lexToken
      case tk of
        L _ EOF -> return [tk]
        _       -> (:) tk <$> loop

lexToken :: P Lexeme
lexToken = do
  inp@(AlexInput loc _) <- getInput
  sc <- getLexState
  case alexScan inp sc of
    AlexEOF -> return $ L (mkRealSrcSpan loc 0) EOF
    AlexError (AlexInput (SrcLoc file row col) _) -> failP $ concat [file , ": lexical error at line ", show row, ", column ", show col]
    AlexSkip inp' len -> setInput inp' >> lexToken
    AlexToken inp' len action -> setInput inp' >> action inp len

tokenOf :: Token -> Action Lexeme
tokenOf tk (AlexInput loc _) len = return $ L (mkRealSrcSpan loc len) tk

getNum :: Action Lexeme
getNum (AlexInput loc@(SrcLoc file row col) buf) len = case B.readInt bstr of
  Nothing -> failP $ concat [file, ": lexical error at line ", show row, ", column ", show col, ": cannot read integer: ", B.unpack bstr]
  Just (i, _) -> return $ L (mkRealSrcSpan loc len) (NUM i)
  where
    bstr = B.take (fromIntegral len) buf

getId :: Action Lexeme
getId (AlexInput loc buf) len = return . L (mkRealSrcSpan loc len) . ID . B.unpack $ B.take (fromIntegral len) buf
}