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
  id    { L _ (ID _)   }
  num   { L _ (NUM _)  }
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

S :: { LStm }
  : S ';' S         { sL1 $2 $ LCompoundStm $1 $3 }
  | id ':=' E       { sL1 $2 $ LAssignStm (retrieveID $1) $3 }
  | print '(' L ')' { sL1 $1 $ LPrintStm $3 }

E :: { LExp }
  : id              { sL1 $1 $ LId (retrieveID $1) }
  | num             { sL1 $1 $ LNum (retrieveNUM $1) }
  | E '+' E         { sL1 $2 $ LPlus $1 $3 }
  | E '-' E         { sL1 $2 $ LMinus $1 $3 }
  | E '*' E         { sL1 $2 $ LTimes $1 $3 }
  | E '/' E         { sL1 $2 $ LDiv $1 $3 }
  | '(' S ',' E ')' { sL1 $3 $ LESeq $2 $4 }

L :: { [LExp] }
  : E               { [$1] }
  | E ',' L         { $1 : $3 }

{

parseError :: Lexeme -> P a
parseError (L span tk) = failP $ concat [srcFile span, ":", show $ srcSRow span, ":", show $ srcSCol span, ": parser error: token = ", show tk]


retrieveID :: Lexeme -> String
retrieveID (L _ (ID id)) = id

retrieveNUM :: Lexeme -> Int
retrieveNUM (L _ (NUM n)) = n
}