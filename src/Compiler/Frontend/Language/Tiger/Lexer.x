{
module Compiler.Frontend.Language.Tiger.Lexer where

import Compiler.Frontend.SrcLoc
import Compiler.Frontend.Lexer
import Data.ByteString.Builder qualified as BB (lazyByteString, intDec, stringUtf8)
import Data.ByteString.Lazy qualified as B (ByteString, take, toStrict)
import Data.ByteString.Lazy.Char8 qualified as B (readInt)
import RIO hiding ( GT, LT, EQ )

}


$whitespace = [\ \t\b]
$newline    = [\n\r\f]
$digit      = 0-9                                            -- digits
$alpha      = [A-Za-z]

@number     = [$digit]+
@identifier = $alpha($alpha|_|$digit)*


tiger:-

<0>            "nil"         { tokenOf NIL }
<0>            "let"         { tokenOf LET }
<0>            "in"          { tokenOf IN }
<0>            "end"         { tokenOf END }
<0>            "type"        { tokenOf TYPE }
<0>            "var"         { tokenOf VAR }
<0>            "function"    { tokenOf FUNCTION }
<0>            "array"       { tokenOf ARRAY }
<0>            "of"          { tokenOf OF }
<0>            "for"         { tokenOf FOR }
<0>            "to"          { tokenOf TO }
<0>            "while"       { tokenOf WHILE }
<0>            "do"          { tokenOf DO }
<0>            "break"       { tokenOf BREAK }
<0>            "if"          { tokenOf IF }
<0>            "then"        { tokenOf THEN }
<0>            "else"        { tokenOf ELSE }
<0>             :\=          { tokenOf ASSIGN }
<0>             \+           { tokenOf PLUS }
<0>             \-           { tokenOf MINUS }
<0>             \*           { tokenOf TIMES }
<0>             \/           { tokenOf DIV }
<0>             \=           { tokenOf EQ }
<0>             \<\>         { tokenOf NEQ }
<0>             \>           { tokenOf GT }
<0>             \<           { tokenOf LT }
<0>             \<\=         { tokenOf LE }
<0>             \>\=         { tokenOf GE }
<0>             &            { tokenOf AND }
<0>             \|           { tokenOf OR }
<0>             \.           { tokenOf DOT }
<0>             \{           { tokenOf LBRACE }
<0>             \}           { tokenOf RBRACE }
<0>             \[           { tokenOf LBRACK }
<0>             \]           { tokenOf RBRACK }
<0>             \)           { tokenOf RPAREN }
<0>             \(           { tokenOf LPAREN }
<0>             :            { tokenOf COLON }
<0>             \;           { tokenOf SEMICOLON }
<0>             ","          { tokenOf COMMA }
<0>             "/*"         { enterNewComment `andBegin` comment }
<0>             @identifier  { getId }
<0>             @number      { getInteger }
<0>             \"[^\"]*\"   { getString } -- TODO: follow the specification of Tiger about string
<0>             $newline     ;
<0>             $whitespace+ ;
<comment>       "/*"         { embedComment }
<comment>       "*/"         { unembedComment }
<comment>       .            ;
<comment>       $newline     ;
<0>             .            { lexerError }

{

data Token =
    EOF
  | NIL
  | ID Text
  | INT Int
  | STRING Text
  | LET
  | IN
  | END
  | TYPE
  | VAR
  | FUNCTION
  | ARRAY
  | OF
  | FOR
  | TO
  | WHILE
  | DO
  | BREAK
  | IF
  | THEN
  | ELSE
  | ASSIGN
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | EQ
  | NEQ
  | GT
  | LT
  | GE
  | LE
  | AND
  | OR
  | DOT
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | LPAREN
  | RPAREN
  | COLON
  | SEMICOLON
  | COMMA
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

getInteger :: Action Lexeme
getInteger (AlexInput loc@(SrcLoc file row col) buf) len = case B.readInt bstr of
  Nothing -> failP . textDisplay . Utf8Builder $ fold [BB.stringUtf8 file, ":", BB.intDec row, ":", BB.intDec col, ": lexer error: cannot read the integer:", BB.lazyByteString bstr]
  Just (i, _) -> pure $ L (mkRealSrcSpan loc len) (INT i)
  where
    bstr = B.take (fromIntegral len) buf

getId :: Action Lexeme
getId (AlexInput loc@(SrcLoc file row col) buf) len = case decodeUtf8' $ B.toStrict bstr of
  Left exception -> failP . textDisplay . Utf8Builder $ fold [BB.stringUtf8 file, ":", BB.intDec row, ":", BB.intDec col, ": lexer error: cannot read the Identifier:", BB.stringUtf8 (show exception), ": ", BB.lazyByteString bstr]
  Right text -> pure . L (mkRealSrcSpan loc len) $ ID text
  where
    bstr = B.take (fromIntegral len) buf


getString :: Action Lexeme
getString (AlexInput loc@(SrcLoc file row col) buf) len = case decodeUtf8' $ B.toStrict bstr of
  Left exception -> failP . textDisplay . Utf8Builder $ fold [BB.stringUtf8 file, ":", BB.intDec row, ":", BB.intDec col, ": lexer error: cannot read the String:", BB.stringUtf8 (show exception), ": ", BB.lazyByteString bstr]
  Right text -> pure . L (mkRealSrcSpan loc len) $ STRING text
  where
    bstr = B.take (fromIntegral len) buf

-- comment action
enterNewComment :: Action Lexeme
enterNewComment _ _ = setCommentDepth 1 >> lexToken

embedComment :: Action Lexeme
embedComment _ _ = modifyCommentDepth (+ 1) >> lexToken

unembedComment :: Action Lexeme
unembedComment _ _  = do
      modifyCommentDepth (subtract 1)
      d <- getCommentDepth
      when (d == 0) (setLexState 0) -- finish comment
      lexToken

}
