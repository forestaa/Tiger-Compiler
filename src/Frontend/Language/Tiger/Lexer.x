{
module Frontend.Language.Tiger.Lexer where

import Prelude hiding ( GT, LT, EQ )
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad

import Frontend.SrcLoc
import Frontend.Lexer

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
  | ID String
  | INT Int
  | STRING String
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
        L _ EOF -> pure [tk]
        _       -> (:) tk <$> loop

lexToken :: P Lexeme
lexToken = do
  inp@(AlexInput loc _) <- getInput
  sc <- getLexState
  case alexScan inp sc of
    AlexEOF -> pure $ L (mkRealSrcSpan loc 0) EOF
    AlexError (AlexInput (SrcLoc file row col) _) -> failP $ concat [file, ":", show row, ":", show col, ": lexer error"]
    AlexSkip inp' _ -> setInput inp' >> lexToken
    AlexToken inp' len action -> setInput inp' >> action inp len

lexerError :: Action a
lexerError (AlexInput (SrcLoc file row col) buf) len = failP $ concat [file, ":", show row, ":", show col, ": lexer error: cannot read the caracter: ", B.unpack $ B.take (fromIntegral len) buf]

tokenOf :: Token -> Action Lexeme
tokenOf tk (AlexInput loc _) len = pure $ L (mkRealSrcSpan loc len) tk

getInteger :: Action Lexeme
getInteger (AlexInput loc@(SrcLoc file row col) buf) len = case B.readInt bstr of
  Nothing -> failP $ concat [file, ":", show row, ":", show col, ": lexer error: cannot read the integer:", B.unpack $ B.take (fromIntegral len) buf]
  Just (i, _) -> pure $ L (mkRealSrcSpan loc len) (INT i)
  where
    bstr = B.take (fromIntegral len) buf

getId :: Action Lexeme
getId (AlexInput loc buf) len = pure . L (mkRealSrcSpan loc len) . ID . B.unpack $ B.take (fromIntegral len) buf


getString :: Action Lexeme
getString (AlexInput loc buf) len = pure . L (mkRealSrcSpan loc len) . STRING . B.unpack $ B.take (fromIntegral len) buf

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
