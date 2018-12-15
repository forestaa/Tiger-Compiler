{
module Linear.Lexer where

import RIO

import qualified Data.ByteString.Lazy.Char8 as B

import SrcLoc
import Lexer.Monad

}


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

type Lexeme = RealLocated Token

-- lexer main
lexer :: (Lexeme -> P a) -> P a
lexer = (>>=) lexToken

scanner :: FilePath -> B.ByteString -> Either String [Lexeme]
scanner = runP loop
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
    AlexError (AlexInput (SrcLoc file row col) _) -> failP $ concat [file, ":", show row, ":", show col, ": lexer error"]
    AlexSkip inp' _ -> setInput inp' >> lexToken
    AlexToken inp' len action -> setInput inp' >> action inp len

lexerError :: Action a
lexerError (AlexInput (SrcLoc file row col) buf) len = failP $ concat [file, ":", show row, ":", show col, ": lexer error: cannot read the caracter: ", B.unpack $ B.take (fromIntegral len) buf]

tokenOf :: Token -> Action Lexeme
tokenOf tk (AlexInput loc _) len = return $ L (mkRealSrcSpan loc len) tk

getNum :: Action Lexeme
getNum (AlexInput loc@(SrcLoc file row col) buf) len = case B.readInt bstr of
  Nothing -> failP $ concat [file, ":", show row, ":", show col, ": lexer error: cannot read the integer:", B.unpack $ B.take (fromIntegral len) buf]
  Just (i, _) -> return $ L (mkRealSrcSpan loc len) (NUM i)
  where
    bstr = B.take (fromIntegral len) buf

getId :: Action Lexeme
getId (AlexInput loc buf) len = return . L (mkRealSrcSpan loc len) . ID . B.unpack $ B.take (fromIntegral len) buf

}