{
module Compiler.Frontend.Language.Tiger.Parser where

import Compiler.Frontend.Id
import Compiler.Frontend.Lexer
import Compiler.Frontend.Language.Tiger.LSyntax
import Compiler.Frontend.Language.Tiger.Lexer
import Compiler.Frontend.SrcLoc
import Prelude hiding (GT, EQ, LT)

}

%name parser
%error { parserError }

%tokentype { Lexeme }

%monad { P }
%lexer { lexer } { L _ EOF }

%token
  'nil'         { L _ NIL }
  'id'          { L _ (ID _) }
  'int'         { L _ (INT _) }
  'string'      { L _ (STRING _) }

  'let'         { L _ LET }
  'in'          { L _ IN }
  'end'         { L _ END }
  'type'        { L _ TYPE }
  'var'         { L _ VAR }
  'function'    { L _ FUNCTION }
  'array'       { L _ ARRAY }
  'of'          { L _ OF }
  'for'         { L _ FOR }
  'to'          { L _ TO }
  'while'       { L _ WHILE }
  'do'          { L _ DO }
  'break'       { L _ BREAK }
  'if'          { L _ IF }
  'then'        { L _ THEN }
  'else'        { L _ ELSE }
  ':='          { L _ ASSIGN }
  '+'           { L _ PLUS }
  '-'           { L _ MINUS }
  '*'           { L _ TIMES }
  '/'           { L _ DIV }
  '='           { L _ EQ }
  '<>'          { L _ NEQ }
  '>'           { L _ GT }
  '<'           { L _ LT }
  '>='          { L _ GE }
  '<='          { L _ LE }
  '&'           { L _ AND }
  '|'           { L _ OR }
  '.'           { L _ DOT }
  '{'           { L _ LBRACE }
  '}'           { L _ RBRACE }
  '['           { L _ LBRACK }
  ']'           { L _ RBRACK }
  ')'           { L _ RPAREN }
  '('           { L _ LPAREN }
  ':'           { L _ COLON }
  ';'           { L _ SEMICOLON }
  ','           { L _ COMMA }


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

parserError :: Lexeme -> P a
parserError (L span tk) = failP $ concat [span.srcFile, ":", show span.srcSRow, ":", show span.srcSCol, ": parser error: token = ", show tk]

retrieveID :: Lexeme -> LId
retrieveID (L loc (ID id)) = L loc id
retrieveINT :: Lexeme -> Int
retrieveINT l = case unLoc l of
                  INT i -> i
retrieveSTRING :: Lexeme -> String
retrieveSTRING l = case unLoc l of
                  STRING str -> str
}
