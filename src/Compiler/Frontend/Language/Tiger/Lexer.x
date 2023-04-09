{
module Compiler.Frontend.Language.Tiger.Lexer where

import Compiler.Frontend.Lexer
import Compiler.Frontend.SrcLoc
import Data.ByteString.Builder qualified as BB (lazyByteString, intDec, stringUtf8)
import Data.ByteString.Lazy qualified as B (ByteString, take, toStrict)
import Data.ByteString.Lazy.Char8 qualified as B (readInt)
import RIO
import RIO.Text qualified as T (decodeUtf8', drop, dropEnd)

}


$whitespace = [\ \t\b]
$newline    = [\n\r\f]
$digit      = 0-9                                            -- digits
$alpha      = [A-Za-z]

@number     = [$digit]+
@identifier = $alpha($alpha|_|$digit)*


tiger:-

<0>            "nil"         { tokenOf Compiler.Frontend.Language.Tiger.Lexer.NIL }
<0>            "let"         { tokenOf Compiler.Frontend.Language.Tiger.Lexer.LET }
<0>            "in"          { tokenOf Compiler.Frontend.Language.Tiger.Lexer.IN }
<0>            "end"         { tokenOf Compiler.Frontend.Language.Tiger.Lexer.END }
<0>            "type"        { tokenOf Compiler.Frontend.Language.Tiger.Lexer.TYPE }
<0>            "var"         { tokenOf Compiler.Frontend.Language.Tiger.Lexer.VAR }
<0>            "function"    { tokenOf Compiler.Frontend.Language.Tiger.Lexer.FUNCTION }
<0>            "array"       { tokenOf Compiler.Frontend.Language.Tiger.Lexer.ARRAY }
<0>            "of"          { tokenOf Compiler.Frontend.Language.Tiger.Lexer.OF }
<0>            "for"         { tokenOf Compiler.Frontend.Language.Tiger.Lexer.FOR }
<0>            "to"          { tokenOf Compiler.Frontend.Language.Tiger.Lexer.TO }
<0>            "while"       { tokenOf Compiler.Frontend.Language.Tiger.Lexer.WHILE }
<0>            "do"          { tokenOf Compiler.Frontend.Language.Tiger.Lexer.DO }
<0>            "break"       { tokenOf Compiler.Frontend.Language.Tiger.Lexer.BREAK }
<0>            "if"          { tokenOf Compiler.Frontend.Language.Tiger.Lexer.IF }
<0>            "then"        { tokenOf Compiler.Frontend.Language.Tiger.Lexer.THEN }
<0>            "else"        { tokenOf Compiler.Frontend.Language.Tiger.Lexer.ELSE }
<0>             :\=          { tokenOf Compiler.Frontend.Language.Tiger.Lexer.ASSIGN }
<0>             \+           { tokenOf Compiler.Frontend.Language.Tiger.Lexer.PLUS }
<0>             \-           { tokenOf Compiler.Frontend.Language.Tiger.Lexer.MINUS }
<0>             \*           { tokenOf Compiler.Frontend.Language.Tiger.Lexer.TIMES }
<0>             \/           { tokenOf Compiler.Frontend.Language.Tiger.Lexer.DIV }
<0>             \=           { tokenOf Compiler.Frontend.Language.Tiger.Lexer.EQ }
<0>             \<\>         { tokenOf Compiler.Frontend.Language.Tiger.Lexer.NEQ }
<0>             \>           { tokenOf Compiler.Frontend.Language.Tiger.Lexer.GT }
<0>             \<           { tokenOf Compiler.Frontend.Language.Tiger.Lexer.LT }
<0>             \<\=         { tokenOf Compiler.Frontend.Language.Tiger.Lexer.LE }
<0>             \>\=         { tokenOf Compiler.Frontend.Language.Tiger.Lexer.GE }
<0>             &            { tokenOf Compiler.Frontend.Language.Tiger.Lexer.AND }
<0>             \|           { tokenOf Compiler.Frontend.Language.Tiger.Lexer.OR }
<0>             \.           { tokenOf Compiler.Frontend.Language.Tiger.Lexer.DOT }
<0>             \{           { tokenOf Compiler.Frontend.Language.Tiger.Lexer.LBRACE }
<0>             \}           { tokenOf Compiler.Frontend.Language.Tiger.Lexer.RBRACE }
<0>             \[           { tokenOf Compiler.Frontend.Language.Tiger.Lexer.LBRACK }
<0>             \]           { tokenOf Compiler.Frontend.Language.Tiger.Lexer.RBRACK }
<0>             \)           { tokenOf Compiler.Frontend.Language.Tiger.Lexer.RPAREN }
<0>             \(           { tokenOf Compiler.Frontend.Language.Tiger.Lexer.LPAREN }
<0>             :            { tokenOf Compiler.Frontend.Language.Tiger.Lexer.COLON }
<0>             \;           { tokenOf Compiler.Frontend.Language.Tiger.Lexer.SEMICOLON }
<0>             ","          { tokenOf Compiler.Frontend.Language.Tiger.Lexer.COMMA }
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

data Token
  = EOF
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

type Lexeme = RealLocated Compiler.Frontend.Language.Tiger.Lexer.Token

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
        L _ Compiler.Frontend.Language.Tiger.Lexer.EOF -> pure [tk]
        _       -> (:) tk <$> loop

lexToken :: P Lexeme
lexToken = do
  inp@(AlexInput loc _) <- getInput
  sc <- getLexState
  case alexScan inp sc of
    AlexEOF -> pure $ L (mkRealSrcSpan loc 0) Compiler.Frontend.Language.Tiger.Lexer.EOF
    AlexError (AlexInput (SrcLoc file row col) buf) -> failP . textDisplay . Utf8Builder $ fold [BB.stringUtf8 file, ":", BB.intDec row, ":", BB.intDec col, ": lexer error: ", BB.lazyByteString buf]
    AlexSkip inp' _ -> setInput inp' >> lexToken
    AlexToken inp' len action -> setInput inp' >> action inp len

lexerError :: Action a
lexerError (AlexInput (SrcLoc file row col) buf) len = failP . textDisplay . Utf8Builder $ fold [BB.stringUtf8 file, ":", BB.intDec row, ":", BB.intDec col, ": lexer error: cannot read the caracter: ", BB.lazyByteString $ B.take (fromIntegral len) buf]

tokenOf :: Compiler.Frontend.Language.Tiger.Lexer.Token -> Action Lexeme
tokenOf tk (AlexInput loc _) len = pure $ L (mkRealSrcSpan loc len) tk

getInteger :: Action Lexeme
getInteger (AlexInput loc@(SrcLoc file row col) buf) len = case B.readInt bstr of
  Nothing -> failP . textDisplay . Utf8Builder $ fold [BB.stringUtf8 file, ":", BB.intDec row, ":", BB.intDec col, ": lexer error: cannot read the integer:", BB.lazyByteString bstr]
  Just (i, _) -> pure $ L (mkRealSrcSpan loc len) (Compiler.Frontend.Language.Tiger.Lexer.INT i)
  where
    bstr = B.take (fromIntegral len) buf

getId :: Action Lexeme
getId (AlexInput loc@(SrcLoc file row col) buf) len = case decodeUtf8' $ B.toStrict bstr of
  Left exception -> failP . textDisplay . Utf8Builder $ fold [BB.stringUtf8 file, ":", BB.intDec row, ":", BB.intDec col, ": lexer error: cannot read the Identifier:", BB.stringUtf8 (show exception), ": ", BB.lazyByteString bstr]
  Right text -> pure . L (mkRealSrcSpan loc len) $ Compiler.Frontend.Language.Tiger.Lexer.ID text
  where
    bstr = B.take (fromIntegral len) buf


getString :: Action Lexeme
getString (AlexInput loc@(SrcLoc file row col) buf) len = case T.decodeUtf8' $ B.toStrict bstr of
  Left exception -> failP . textDisplay . Utf8Builder $ fold [BB.stringUtf8 file, ":", BB.intDec row, ":", BB.intDec col, ": lexer error: cannot read the String:", BB.stringUtf8 (show exception), ": ", BB.lazyByteString bstr]
  Right text -> pure . L (mkRealSrcSpan loc len) $ Compiler.Frontend.Language.Tiger.Lexer.STRING (T.drop 1 $ T.dropEnd 1 text) -- TODO: it is better to add QUOTE to Token
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
