{
module Compiler.Frontend.Language.Linear.Lexer where

import Compiler.Frontend.Lexer
import Compiler.Frontend.SrcLoc
import Data.ByteString.Builder qualified as BB (lazyByteString, intDec, stringUtf8)
import Data.ByteString.Lazy qualified as B (ByteString, take, toStrict)
import Data.ByteString.Lazy.Char8 qualified as B (readInt)
import RIO hiding ( GT, LT, EQ )

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
  | ID Text
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

instance Display Token where
  display = displayShow

type Lexeme = RealLocated Token

-- lexer main
lexer :: (Lexeme -> P a) -> P a
lexer = (>>=) lexToken

scanner :: FilePath -> B.ByteString -> Either ParserException [Lexeme]
scanner = runP loop
  where
    loop :: P [Lexeme]
    loop = do
      tk <- lexToken
      case tk of
        L _ EOF -> pure [tk]
        _       -> (:) tk <$> loop

lexToken :: P Lexeme
lexToken = do
  inp@(AlexInput loc _) <- getInput
  sc <- getLexState
  case alexScan inp sc of
    AlexEOF -> pure $ L (mkRealSrcSpan loc 0) EOF
    AlexError (AlexInput (SrcLoc file row col) buf) -> failP . textDisplay . Utf8Builder $ fold [BB.stringUtf8 file, ":", BB.intDec row, ":", BB.intDec col, ": lexer error: ", BB.lazyByteString buf]
    AlexSkip inp' _ -> setInput inp' >> lexToken
    AlexToken inp' len action -> setInput inp' >> action inp len

lexerError :: Action a
lexerError (AlexInput (SrcLoc file row col) buf) len = failP . textDisplay . Utf8Builder $ fold [BB.stringUtf8 file, ":", BB.intDec row, ":", BB.intDec col, ": lexer error: cannot read the caracter: ", BB.lazyByteString $ B.take (fromIntegral len) buf]

tokenOf :: Token -> Action Lexeme
tokenOf tk (AlexInput loc _) len = pure $ L (mkRealSrcSpan loc len) tk

getNum :: Action Lexeme
getNum (AlexInput loc@(SrcLoc file row col) buf) len = case B.readInt bstr of
  Nothing -> failP . textDisplay . Utf8Builder $ fold [BB.stringUtf8 file, ":", BB.intDec row, ":", BB.intDec col, ": lexer error: cannot read the integer:", BB.lazyByteString bstr]
  Just (i, _) -> pure $ L (mkRealSrcSpan loc len) (NUM i)
  where
    bstr = B.take (fromIntegral len) buf

getId :: Action Lexeme
getId (AlexInput loc@(SrcLoc file row col) buf) len = case decodeUtf8' $ B.toStrict bstr of
  Left exception -> failP . textDisplay . Utf8Builder $ fold [BB.stringUtf8 file, ":", BB.intDec row, ":", BB.intDec col, ": lexer error: cannot read the Identifier:", BB.stringUtf8 (show exception), ": ", BB.lazyByteString bstr]
  Right text -> pure . L (mkRealSrcSpan loc len) $ ID text
  where
    bstr = B.take (fromIntegral len) buf

}
