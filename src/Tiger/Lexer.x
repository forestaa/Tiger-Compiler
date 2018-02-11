{
-- {-# OPTIONS -w -funbox-strict-fields #-}
module Tiger.Lexer (
                   lexer, Lexeme (..), LexemeClass (..)
                  , Pos, Alex, getCollNameToIdent, getParserCurrentToken, putCollNameToIdent
                  , getParserPos, putParserPos
                  , alexError, runAlex, runAlexTable, alexGetInput, showPosn
                  , line_number
                  , scanner
                  ) where

import Prelude hiding ( GT, LT, EQ )
import Control.Monad
import Data.Maybe
import Data.Map ( Map )
import qualified Data.Map as Map ( empty )

import Debug.Trace
}

%wrapper "monadUserState"

$whitespace = [\ \t\b]
$digit      = 0-9                                            -- digits
$alpha      = [A-Za-z]
$letter     = [a-zA-Z]                                       -- alphabetic characters
$ident      = [$letter $digit _]                             -- identifier character

@number     = [$digit]+
@identifier = $alpha($alpha|_|$digit)*


tiger:-

<0>            "type"        { tokenOf TYPE }
<0>            "var"         { tokenOf VAR }
<0>            "function"    { tokenOf FUNCTION }
<0>            "break"       { tokenOf BREAK }
<0>            "of"          { tokenOf OF }
<0>            "end"         { tokenOf END }
<0>            "in"          { tokenOf IN }
<0>            "nil"         { tokenOf NIL }
<0>            "let"         { tokenOf LET }
<0>            "do"          { tokenOf DO }
<0>            "to"          { tokenOf TO }
<0>            "for"         { tokenOf FOR }
<0>            "while"       { tokenOf WHILE }
<0>            "else"        { tokenOf ELSE }
<0>            "then"        { tokenOf THEN }
<0>            "if"          { tokenOf IF }
<0>            "array"       { tokenOf ARRAY }
<0>             :\=          { tokenOf ASSIGN }
<0>             \|           { tokenOf OR }
<0>             &            { tokenOf AND }
<0>             \>\=         { tokenOf GE }
<0>             \>           { tokenOf GT }
<0>             \<\=         { tokenOf LE }
<0>             \<           { tokenOf LT }
<0>             \<\>         { tokenOf NEQ }
<0>             \=           { tokenOf EQ }
<0>             \/           { tokenOf DIVIDE }
<0>             \*           { tokenOf TIMES }
<0>             \-           { tokenOf MINUS }
<0>             \+           { tokenOf PLUS }
<0>             \.           { tokenOf DOT }
<0>             \}           { tokenOf RBRACE }
<0>             \{           { tokenOf LBRACE }
<0>             \[           { tokenOf LBRACK }
<0>             \]           { tokenOf RBRACK }
<0>             \)           { tokenOf RPAREN }
<0>             \(           { tokenOf LPAREN }
<0>             \;           { tokenOf SEMICOLON }
<0>             :            { tokenOf COLON }
<0>             ","          { tokenOf COMMA }
<0>             "/*"         { enterNewComment `andBegin` comment }
<comment> "/*"         { embedComment }
<comment> "*/"         { unembedComment }
<comment> .            ;
<comment> \n           { skip }
<0>             \"           { begin string }
<string>  \\n          { addCharToString '\n' }
<string>  \"           { leaveString `andBegin` 0 }
<string>  .            { addCurrentToString }
<string>  \n           { addCharToString '\n' }
<0>             \n           { skip }
<0>             $whitespace+ ;
<0>             @number      { getInteger }
<0>             @identifier  { getId }
<0>             .            { lexerError }

{

type Letters = Maybe String
showLetters :: Letters -> String
showLetters Nothing  = ""
showLetters (Just s) = "string = " ++ show s

data Lexeme = Lexeme AlexPosn LexemeClass Letters
instance Show Lexeme where
  show (Lexeme _ EOF _)   = "  Lexeme EOF"
  show (Lexeme p cl mbs) = concat ["  Lexeme class = ", show cl, " posn = ", showPosn p, showLetters mbs]

showPosn :: AlexPosn -> String
showPosn (AlexPn _ row col) = concat ["(row=", show row, ",", "col=", show col, ")"]

-- tokPosn :: Lexeme -> AlexPosn
-- tokPosn (Lexeme p _ _) = p

data LexemeClass =
        EOF
      | ID         String
      | INT        Int
      | STRING     String
      | COMMA
      | COLON
      | SEMICOLON
      | LPAREN
      | RPAREN
      | LBRACK
      | RBRACK
      | LBRACE
      | RBRACE
      | DOT
      | PLUS
      | MINUS
      | TIMES
      | DIVIDE
      | EQ
      | NEQ
      | LT
      | LE
      | GT
      | GE
      | AND
      | OR
      | ASSIGN
      | ARRAY
      | IF
      | THEN
      | ELSE
      | WHILE
      | FOR
      | TO
      | DO
      | LET
      | IN
      | END
      | OF
      | BREAK
      | NIL
      | FUNCTION
      | VAR
      | TYPE
      | UNARYMINUS
      deriving (Show, Eq)

eof = Lexeme undefined EOF Nothing
type Pos     = Maybe AlexPosn
data AlexUserState = AlexUserState
                   {
                     -- used by the lexer phase
                       lexerCommentDepth  :: Int
                     , lexerStringValue   :: String
                     -- used by the parser phase
                     , parserCollIdent    :: Map String Int
                     , parserCurrentToken :: Lexeme
                     , parserPos          :: Pos
                   }
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
                   {
                       lexerCommentDepth  = 0
                     , lexerStringValue   = ""
                     , parserCollIdent    = Map.empty
                     , parserCurrentToken = eof
                     , parserPos          = Nothing
                   }


alexEOF :: Alex Lexeme
alexEOF = return eof

-- unit test
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

getInteger :: AlexInput -> Int -> Alex Lexeme
getInteger (p, _, _, input) len = return $ Lexeme p (INT $ read str) (Just str)
  where
    str = take len input

getId :: AlexInput -> Int -> Alex Lexeme
getId (p, _, _, input) len = return $ Lexeme p (ID str) (Just str)
  where
    str = take len input


-- comment action
enterNewComment :: AlexAction Lexeme
enterNewComment _ _ = putLexerCommentDepth 1 >> alexMonadScan

embedComment :: AlexAction Lexeme
embedComment _ _ = modifyLexerCommentDepth (+ 1) >> alexMonadScan

unembedComment :: AlexAction Lexeme
unembedComment _ _  =
    do d <- modifyLexerCommentDepth (subtract 1)
       when (d == 0) (alexSetStartCode 0) -- finish comment
       alexMonadScan

getLexerCommentDepth :: Alex Int
getLexerCommentDepth = Alex $ \s@AlexState{alex_ust = ust} -> Right (s, lexerCommentDepth ust)

putLexerCommentDepth :: Int -> Alex ()
putLexerCommentDepth d = Alex $ \s -> Right (s{alex_ust = (alex_ust s){lexerCommentDepth = d}}, ())

modifyLexerCommentDepth :: (Int -> Int) -> Alex Int
modifyLexerCommentDepth f = Alex $ \s@AlexState{alex_ust = ust} ->
  let d = f $ lexerCommentDepth ust
  in Right (s{alex_ust = ust{lexerCommentDepth = d}}, d)


-- string action
addCharToString :: Char -> AlexAction Lexeme
addCharToString c _ _ = addCharToLexerStringValue c >> alexMonadScan

addCurrentToString :: AlexAction Lexeme
addCurrentToString (_, _, _, input) len = modifyLexerStringValue (++ take len input) >> alexMonadScan

leaveString :: AlexAction Lexeme
leaveString (p, _, _, input) len =
    do s <- getLexerStringValue
       putLexerStringValue ""
       return $ Lexeme p (STRING s) (Just s)

getLexerStringValue :: Alex String
getLexerStringValue = Alex $ \s@AlexState{alex_ust = ust} -> Right (s, lexerStringValue ust)

putLexerStringValue :: String -> Alex ()
putLexerStringValue str = Alex $ \s -> Right (s{alex_ust = (alex_ust s){lexerStringValue = str}}, ())

modifyLexerStringValue :: (String -> String) -> Alex String
modifyLexerStringValue f = Alex $ \s@AlexState{alex_ust = ust} ->
  let str = f $ lexerStringValue ust
  in Right (s{alex_ust = ust{lexerStringValue = str}}, str)

addCharToLexerStringValue :: Char -> Alex ()
addCharToLexerStringValue c = modifyLexerStringValue (++ [c]) >> return ()


-- for parser
getCollNameToIdent :: Alex (Map String Int)
getCollNameToIdent = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, parserCollIdent ust)

putCollNameToIdent :: Map String Int -> Alex ()
putCollNameToIdent ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){parserCollIdent=ss}}, ())

getParserCurrentToken :: Alex Lexeme
getParserCurrentToken = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, parserCurrentToken ust)

putParserCurrentToken :: Lexeme -> Alex ()
putParserCurrentToken token = Alex $ \s -> Right (s{alex_ust=(alex_ust s){parserCurrentToken = token}}, ())

getParserPos :: Alex Pos
getParserPos = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, parserPos ust)

putParserPos :: Pos -> Alex ()
putParserPos ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){parserPos=ss}}, ())

-- why does this function number destruct Maybe computation?
line_number :: Pos -> (Int, Int)
line_number Nothing                   = (0, 0)
line_number (Just (AlexPn _ lig col)) = (lig, col)


-- lexer main
lexer :: (Lexeme -> Alex a) -> Alex a
lexer cont =
    do t <- lexToken
       putParserCurrentToken t  -- helps in producing informative error messages
       cont t
lexToken :: Alex Lexeme
lexToken =
    do
       inp <- alexGetInput
       sc <- alexGetStartCode
       case alexScan inp sc of
            AlexEOF              -> alexEOF
            AlexError _          -> alexError "lexical error"
            AlexSkip  inp1 _     -> do
                                       alexSetInput inp1
                                       lexToken
            AlexToken inp1 len t -> do
                                       alexSetInput inp1
                                       t inp len

-- used by the parser: run lexer, parser & get the symbol table
runAlexTable :: String -> Alex a -> Either String (a, Map String Int)
runAlexTable input (Alex f)
   = case f (AlexState { alex_pos = alexStartPos
                       , alex_inp = input
                       , alex_chr = '\n'
                       , alex_scd = 0
                       , alex_ust = alexInitUserState }) of
            Left msg      -> Left msg
            Right (st, a) -> Right (a, parserCollIdent (alex_ust st))
}