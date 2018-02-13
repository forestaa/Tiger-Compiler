{
module Linear.Parser where

import Linear.Syntax
import Linear.Lexer
import Lexer.Monad
import SrcLoc
}

%name parser
%error { parseError }

%tokentype { Lexeme }

%monad { P }
%lexer { lexer } { L _ EOF }

%left '+' '-'
%left '*' '/'


%token
  id    { L _ (ID $$)   }
  num   { L _ (NUM $$)  }
  print { L _ PRINT     }

  ','   { L _ COMMA     }
  ';'   { L _ SEMICOLON }
  '('   { L _ LPAREN    }
  ')'   { L _ RPAREN    }
  '+'   { L _ PLUS      }
  '-'   { L _ MINUS     }
  '*'   { L _ TIMES     }
  '/'   { L _ DIVIDE    }
  ':='  { L _ ASSIGN    }

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

parseError :: Lexeme -> P a
parseError (L span tk) = failP $ concat [srcFile span, ":", show $ srcSRow span, ":", show $ srcSCol span, ": parser error: token = ", show tk]

}