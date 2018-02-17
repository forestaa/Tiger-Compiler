{
module Tiger.Parser where

import Prelude hiding (GT, EQ, LT)

import Tiger.Syntax
import Tiger.Lexer
import Lexer.Monad
import SrcLoc
}

%name parser
%error { parserError }

%tokentype { Lexeme }

%monad { P }
%lexer { lexer } { L _ EOF }

%token
  'string'   { L _ (STRING _) }
  'int'         { L _ (INT _) }
  'id'  { L _ (ID _) }

  'type' { L _ TYPE }
  'var'  { L _ VAR }
  'function'    { L _ FUNCTION }
  'break'       { L _ BREAK }
  'of'          { L _ OF }
  'end'         { L _ END }
  'in'          { L _ IN }
  'nil'         { L _ NIL }
  'let'         { L _ LET }
  'do'          { L _ DO }
  'to'          { L _ TO }
  'for'         { L _ FOR }
  'while'       { L _ WHILE }
  'else'        { L _ ELSE }
  'then'        { L _ THEN }
  'if'          { L _ IF }
  'array'       { L _ ARRAY }
  ':='          { L _ ASSIGN }
  '|'           { L _ OR }
  '&'            { L _ AND }
  '>='         { L _ GE }
  '>'           { L _ GT }
  '<='         { L _ LE }
  '<'           { L _ LT }
  '<>'         { L _ NEQ }
  '='           { L _ EQ }
  '/'           { L _ DIV }
  '*'           { L _ TIMES }
  '-'           { L _ MINUS }
  '+'           { L _ PLUS }
  '.'           { L _ DOT }
  '{'           { L _ LBRACE }
  '}'           { L _ RBRACE }
  '['           { L _ LBRACK }
  ']'           { L _ RBRACK }
  ')'           { L _ RPAREN }
  '('           { L _ LPAREN }
  ';'           { L _ SEMICOLON }
  ':'            { L _ COLON }
  ','          { L _ COMMA }

%right ':='
%left '|'
%left '&'
%nonassoc '=' '<>' '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/'

%left UMINUS

%%

exp :: { LExp }
  : 'nil'                         { sL1 $1 Nil }
  | 'int'                         { sL1 $1 . Int $ retrieveINT $1 }
  | 'string'                      { sL1 $1 . String $ retrieveSTRING $1 }

  | 'id' '[' exp ']' 'of' exp  { sL2 $1 $6 $ ArrayCreate (retrieveID $1) $3 $6 }
  | 'id' '{' fieldassigns '}'  { sL2 $1 $4 $ RecordCreate (retrieveID $1) (reverse $3) }

  | lvalue                        { sL1 $1 $ Var $1 }

  | 'id' '(' args ')'             { sL2 $1 $4 $ FunApply (retrieveID $1) (reverse $3) }

  | '-' exp     %prec UMINUS      { sL2 $1 $2 $ Op (sL1 $1 $ Int 0) Minus $2 }
  | exp op exp                    { sL2 $1 $3 $ Op $1 $2 $3 }
  | exp '&' exp                   { sL2 $1 $3 $ If $1 $3 (Just . sL1 $2 $ Int 0) }
  | exp '|' exp                   { sL2 $1 $3 $ If $1 (sL1 $2 $ Int 1) (Just $3) }
  | '(' exps ')'                   { sL2 $1 $3 $ Seq $2 }

  | lvalue ':=' exp               { sL2 $1 $3 $ Assign $1 $3 }

  | 'if' exp 'then' exp            { sL2 $1 $4 $ If $2 $4 Nothing }
  | 'if' exp 'then' exp 'else' exp { sL2 $1 $6 $ If $2 $4 (Just $6) }
  | 'while' exp 'do' exp           { sL2 $1 $4 $ While $2 $4 }
  | 'for' 'id' ':=' exp 'to' exp 'do' exp { sL2 $1 $8 $ For (retrieveID $2) $4 $6 $8 }
  | 'break'                        { sL1 $1 Break }
  | 'let' decs 'in' exps 'end'     { sL2 $1 $5 $ Let (reverse $2) (reverse $4) }

fieldassigns :: { [LFieldAssign] }
  : 'id' '=' exp                  { [sL2 $1 $3 $ FieldAssign (retrieveID $1) $3] }
  | fieldassigns ',' 'id' '=' exp { (sL2 $3 $5 $ FieldAssign (retrieveID $3) $5) : $1 } -- left recursion

lvalue :: { LValue }
  : 'id'                 { sL1 $1 $ Id (retrieveID $1) }
  | lvalue '.' 'id'      { sL2 $1 $3 $ RecField $1 (retrieveID $3) }
  | lvalue '[' exp ']'   { sL2 $1 $4 $ ArrayIndex $1 $3 }

args :: { [LExp] }
  : {- empty -}          { [] }
  | exp                  { [$1] }
  | args ',' exp         { $3 : $1 } -- left recursion

op :: { Op }
  : '+'                  { Plus }
  | '-'                  { Minus }
  | '*'                  { Times }
  | '/'                  { Div }
  | '='                  { Eq }
  | '<>'                 { NEq }
  | '>'                  { Gt }
  | '<'                  { Lt }
  | '>='                 { Ge }
  | '<='                 { Le }

decs :: { [LDec] }
  : {- empty -}          { [] }
  | decs dec             { $2 : $1 } -- left recursion

dec :: { LDec }
  : 'type' 'id' '=' type             { sL2 $1 $4 $ TypeDec (retrieveID $2) $4 }
  | 'var' 'id' ':' 'id' ':=' exp  { sL2 $1 $6 $ VarDec (retrieveID $2) (Just (retrieveID $4)) $6 }
  | 'var' 'id' ':=' exp              { sL2 $1 $4 $ VarDec (retrieveID $2) Nothing $4 }
  | 'function' 'id' '(' tyfields ')' ':' 'id' '=' exp  { sL2 $1 $9 $ FunDec (retrieveID $2) $4 (Just $ retrieveID $7) $9 }
  | 'function' 'id' '(' tyfields ')' '=' exp { sL2 $1 $7 $ FunDec (retrieveID $2) (reverse $4) Nothing $7 }

type :: { LType }
  : 'id'              { sL1 $1 . TypeId $ retrieveID $1 }
  | '{' tyfields '}'       { sL2 $1 $3 $ RecordType $2 }
  | 'array' 'of' 'id' { sL2 $1 $3 $ ArrayType (retrieveID $3) }

tyfields :: { [LField] }
  : {- empty -}          { [] }
  | 'id' ':' 'id'     { [sL2 $1 $3 $ Field (retrieveID $1) (retrieveID $3)] }
  | tyfields ',' 'id' ':' 'id' { (sL2 $3 $5 $ Field (retrieveID $3) (retrieveID $5) ) : $1} -- left recursion

exps :: { [LExp] }
  : {- empty -}          { [] }
  | exp                  { [$1] }
  | exps ';' exp         { $3 : $1 }  -- left recursion


{

parserError :: Lexeme -> P a
parserError (L span tk) = failP $ concat [srcFile span, ":", show $ srcSRow span, ":", show $ srcSCol span, ": parser error: token = ", show tk]

retrieveID :: Lexeme -> Id
retrieveID l = case unLoc l of
                  ID id -> id
retrieveINT :: Lexeme -> Int
retrieveINT l = case unLoc l of
                  INT i -> i
retrieveSTRING :: Lexeme -> String
retrieveSTRING l = case unLoc l of
                  STRING str -> str
}