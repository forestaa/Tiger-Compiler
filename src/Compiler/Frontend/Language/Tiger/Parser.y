{
module Compiler.Frontend.Language.Tiger.Parser where

import Compiler.Frontend.Id
import Compiler.Frontend.Lexer
import Compiler.Frontend.Language.Tiger.Lexer qualified as Lexer
import Compiler.Frontend.Language.Tiger.LSyntax
import Compiler.Frontend.SrcLoc
import Data.ByteString.Builder qualified as BB (stringUtf8)
import Prelude hiding (GT, EQ, LT)
import RIO (Text, Display(..), displayShow, fold, Utf8Builder(..))

}

%name parser
%error { parserError }

%tokentype { Lexer.Lexeme }

%monad { P }
%lexer { Lexer.lexer } { L _ Lexer.EOF }

%token
  'nil'         { L _ Lexer.NIL }
  'id'          { L _ (Lexer.ID _) }
  'int'         { L _ (Lexer.INT _) }
  'string'      { L _ (Lexer.STRING _) }

  'let'         { L _ Lexer.LET }
  'in'          { L _ Lexer.IN }
  'end'         { L _ Lexer.END }
  'type'        { L _ Lexer.TYPE }
  'var'         { L _ Lexer.VAR }
  'function'    { L _ Lexer.FUNCTION }
  'array'       { L _ Lexer.ARRAY }
  'of'          { L _ Lexer.OF }
  'for'         { L _ Lexer.FOR }
  'to'          { L _ Lexer.TO }
  'while'       { L _ Lexer.WHILE }
  'do'          { L _ Lexer.DO }
  'break'       { L _ Lexer.BREAK }
  'if'          { L _ Lexer.IF }
  'then'        { L _ Lexer.THEN }
  'else'        { L _ Lexer.ELSE }
  ':='          { L _ Lexer.ASSIGN }
  '+'           { L _ Lexer.PLUS }
  '-'           { L _ Lexer.MINUS }
  '*'           { L _ Lexer.TIMES }
  '/'           { L _ Lexer.DIV }
  '='           { L _ Lexer.EQ }
  '<>'          { L _ Lexer.NEQ }
  '>'           { L _ Lexer.GT }
  '<'           { L _ Lexer.LT }
  '>='          { L _ Lexer.GE }
  '<='          { L _ Lexer.LE }
  '&'           { L _ Lexer.AND }
  '|'           { L _ Lexer.OR }
  '.'           { L _ Lexer.DOT }
  '{'           { L _ Lexer.LBRACE }
  '}'           { L _ Lexer.RBRACE }
  '['           { L _ Lexer.LBRACK }
  ']'           { L _ Lexer.RBRACK }
  ')'           { L _ Lexer.RPAREN }
  '('           { L _ Lexer.LPAREN }
  ':'           { L _ Lexer.COLON }
  ';'           { L _ Lexer.SEMICOLON }
  ','           { L _ Lexer.COMMA }


%nonassoc 'then' 'do' 'of'
%nonassoc 'else'

%right ':='
%left '|'
%left '&'
%nonassoc '=' '<>' '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/'

%left UMINUS

%%

exp :: { LExp }
  : 'nil'                                 { sL1 $1 Nil }
  | 'int'                                 { sL1 $1 . Int $ retrieveINT $1 }
  | 'string'                              { sL1 $1 . String $ retrieveSTRING $1 }

  | 'id' '[' exp ']' 'of' exp             { sL2 $1 $6 $ ArrayCreate (retrieveID $1) $3 $6 }
  | 'id' '{' fieldassigns '}'             { sL2 $1 $4 $ RecordCreate (retrieveID $1) $3 }

  | lvalue                                { sL1 $1 $ Var $1 }

  | 'id' '(' args ')'                     { sL2 $1 $4 $ FunApply (retrieveID $1) $3 }

  | '-' exp     %prec UMINUS              { sL2 $1 $2 $ Op (sL1 $1 $ Int 0) (sL1 $1 Minus) $2 }
  | exp '+' exp                           { sL2 $1 $3 $ Op $1 (sL1 $2 Plus) $3 }
  | exp '-' exp                           { sL2 $1 $3 $ Op $1 (sL1 $2 Minus) $3 }
  | exp '*' exp                           { sL2 $1 $3 $ Op $1 (sL1 $2 Times) $3 }
  | exp '/' exp                           { sL2 $1 $3 $ Op $1 (sL1 $2 Div) $3 }
  | exp '=' exp                           { sL2 $1 $3 $ Op $1 (sL1 $2 Eq) $3 }
  | exp '<>' exp                          { sL2 $1 $3 $ Op $1 (sL1 $2 NEq) $3 }
  | exp '>' exp                           { sL2 $1 $3 $ Op $1 (sL1 $2 Gt) $3 }
  | exp '<' exp                           { sL2 $1 $3 $ Op $1 (sL1 $2 Lt) $3 }
  | exp '>=' exp                          { sL2 $1 $3 $ Op $1 (sL1 $2 Ge) $3 }
  | exp '<=' exp                          { sL2 $1 $3 $ Op $1 (sL1 $2 Le) $3 }
  | exp '&' exp                           { sL2 $1 $3 $ If $1 $3 (Just . sL1 $2 $ Int 0) }
  | exp '|' exp                           { sL2 $1 $3 $ If $1 (sL1 $2 $ Int 1) (Just $3) }
  | '(' exps ')'                          { sL2 $1 $3 $ Seq $2 }

  | lvalue ':=' exp                       { sL2 $1 $3 $ Assign $1 $3 }

  | 'if' exp 'then' exp                   { sL2 $1 $4 $ If $2 $4 Nothing }
  | 'if' exp 'then' exp 'else' exp        { sL2 $1 $6 $ If $2 $4 (Just $6) }
  | 'while' exp 'do' exp                  { sL2 $1 $4 $ While $2 $4 }
  | 'for' 'id' ':=' exp 'to' exp 'do' exp { sL2 $1 $8 $ For (retrieveID $2) False $4 $6 $8 }
  | 'break'                               { sL1 $1 Break }
  | 'let' decs 'in' exps 'end'            { sL2 $1 $5 $ Let $2 (sL2 $3 $5 $ Seq $4) }

fieldassigns :: { [LFieldAssign] }
  : {- empty -}                           { [] }
  | 'id' '=' exp                          { [sL2 $1 $3 $ FieldAssign (retrieveID $1) $3] }
  | fieldassigns ',' 'id' '=' exp         { foldr (:) [sL2 $3 $5 $ FieldAssign (retrieveID $3) $5] $1 } -- left recursion


-- prevent shift/reduce conflict
-- original grammer is the following:
-- lvalue -> 'id'
--        -> lvalue '[' exp ']'
--        -> lvalue '.' 'id'
-- This cause the following shift/reduce conflict.
-- exp -> 'id' . '[' exp ']' 'of' exp
-- lvalue -> 'id' .
-- To resolve the conflict, adopt this redundant grammer and defer the reduce.

lvalue :: { LValue }
  : 'id'                      { sL1 $1 $ Id (retrieveID $1) }
  | lvalue_record             { $1 }
  | lvalue_array              { $1 }

lvalue_record :: { LValue }
  : 'id' '.' 'id'             { sL2 $1 $3 $ RecField (sL1 $1 $ Id (retrieveID $1)) (retrieveID $3) }
  | lvalue_array '.' 'id'     { sL2 $1 $3 $ RecField $1 (retrieveID $3) }
  | lvalue_record '.' 'id'    { sL2 $1 $3 $ RecField $1 (retrieveID $3) }

lvalue_array :: { LValue }
  : 'id' '[' exp ']'          { sL2 $1 $4 $ ArrayIndex (sL1 $1 $ Id (retrieveID $1)) $3 }
  | lvalue_array '[' exp ']'  { sL2 $1 $4 $ ArrayIndex $1 $3 }
  | lvalue_record '[' exp ']' { sL2 $1 $4 $ ArrayIndex $1 $3 }


args :: { [LExp] }
  : {- empty -}          { [] }
  | exp                  { [$1] }
  | args ',' exp         { foldr (:) [$3] $1 } -- left recursion

decs :: { [LDec] }
  : {- empty -}          { [] }
  | decs dec             { foldr (:) [$2] $1 } -- left recursion

dec :: { LDec }
  : 'type' 'id' '=' type                               { sL2 $1 $4 $ TypeDec (retrieveID $2) $4 }
  | 'var' 'id' ':' 'id' ':=' exp                       { sL2 $1 $6 $ VarDec (retrieveID $2) False (Just (retrieveID $4)) $6 }
  | 'var' 'id' ':=' exp                                { sL2 $1 $4 $ VarDec (retrieveID $2) False Nothing $4 }
  | 'function' 'id' '(' tyfields ')' ':' 'id' '=' exp  { sL2 $1 $9 $ FunDec (retrieveID $2) $4 (Just $ retrieveID $7) $9 }
  | 'function' 'id' '(' tyfields ')' '=' exp           { sL2 $1 $7 $ FunDec (retrieveID $2) $4 Nothing $7 }

type :: { LType }
  : 'id'                 { sL1 $1 . TypeId $ retrieveID $1 }
  | '{' tyfields '}'     { sL2 $1 $3 $ RecordType $2 }
  | 'array' 'of' 'id'    { sL2 $1 $3 $ ArrayType (retrieveID $3) }

tyfields :: { [LField] }
  : {- empty -}                { [] }
  | 'id' ':' 'id'              { [sL2 $1 $3 $ Field (retrieveID $1) False (retrieveID $3)] }
  | tyfields ',' 'id' ':' 'id' { foldr (:) [sL2 $3 $5 $ Field (retrieveID $3) False (retrieveID $5)] $1} -- left recursion

exps :: { [LExp] }
  : {- empty -}          { [] }
  | exp                  { [$1] }
  | exps ';' exp         { foldr (:) [$3] $1 }  -- left recursion


{

parserError :: Lexer.Lexeme -> P a
parserError (L span tk) = failP . textDisplay $ fold [Utf8Builder (BB.stringUtf8 span.srcFile), ":", display span.srcSRow, ":", display span.srcSCol, ": parser error: token = ", display tk]

retrieveID :: Lexer.Lexeme -> LId
retrieveID (L loc (Lexer.ID id)) = L loc id
retrieveINT :: Lexer.Lexeme -> Int
retrieveINT l = case unLoc l of
                  Lexer.INT i -> i
retrieveSTRING :: Lexer.Lexeme -> Text
retrieveSTRING l = case unLoc l of
                  Lexer.STRING str -> str

}
