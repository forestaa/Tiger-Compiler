{
module Linear.Parser where

import Linear.Linear
import Linear.Lexer

}

%name parser
%error { parseError }

%tokentype { Lexeme }

%monad { Alex }
%lexer { lexer } { Lexeme _ EOF _ }

%left '+' '-'
%left '*' '/'


%token
  id    { Lexeme _ (ID $$)  _ }
  num   { Lexeme _ (NUM $$) _ }
  print { Lexeme _ PRINT    _ }

  ','   { Lexeme _ COMMA    _ }
  ';'   { Lexeme _ SEMICOLON _ }
  '('   { Lexeme _ LPAREN    _ }
  ')'   { Lexeme _ RPAREN    _ }
  '+'   { Lexeme _ PLUS      _ }
  '-'   { Lexeme _ MINUS     _ }
  '*'   { Lexeme _ TIMES     _ }
  '/'   { Lexeme _ DIVIDE    _ }
  ':='  { Lexeme _ ASSIGN    _ }

%%

S :: { Stm }
  : S ';' S         { CompoundStm $1 $3 }
  | id ':=' E       { AssignStm $1 $3 }
  | print '(' L ')' { PrintStm $3 }

E :: { Exp }
  : id              { Id $1 }
  | num             { Num $1 }
  | E '+' E         { BiOp $1 Plus $3 }
  | E '-' E         { BiOp $1 Minus $3 }
  | E '*' E         { BiOp $1 Times $3 }
  | E '/' E         { BiOp $1 Div $3 }
  | '(' S ',' E ')' { Eseq $2 $4 }

L :: { [Exp] }
  : E               { [$1] }
  | E ',' L         { $1 : $3 }

{
parseError :: Lexeme -> Alex a
parseError _ = error "parse Error"

}