{
module Linear.Parser where

import Linear.Syntax
import Linear.Lexer
import Lexer.Monad
import SrcLoc
}

%name parser
%error { parserError }

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
  : S ';' S         { sL1 $2 $ CompoundStm $1 $3 }
  | id ':=' E       { sL2 $1 $2 $ AssignStm (retrieveID $1) $3 }
  | print '(' L ')' { sL1 $1 $ PrintStm (reverse $3) }

E :: { LExp }
  : id              { sL1 $1 $ Id (retrieveID $1) }
  | num             { sL1 $1 $ Num (retrieveNUM $1) }
  | E '+' E         { sL1 $2 $ Plus $1 $3 }
  | E '-' E         { sL1 $2 $ Minus $1 $3 }
  | E '*' E         { sL1 $2 $ Times $1 $3 }
  | E '/' E         { sL1 $2 $ Div $1 $3 }
  | '(' S ',' E ')' { sL1 $3 $ ESeq $2 $4 }

L :: { [LExp] }
  : E               { [$1] }
  | L ',' E         { $3 : $1 } -- left recursion

{

parserError :: Lexeme -> P a
parserError (L span tk) = failP $ concat [srcFile span, ":", show $ srcSRow span, ":", show $ srcSCol span, ": parser error: token = ", show tk]


retrieveID :: Lexeme -> String
retrieveID (L _ (ID id)) = id

retrieveNUM :: Lexeme -> Int
retrieveNUM (L _ (NUM n)) = n
}