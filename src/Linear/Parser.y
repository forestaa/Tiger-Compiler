{
module Linear.Parser where

import RIO
import RIO.List.Partial ((!!))

import Id
import SrcLoc
import Lexer.Monad

import Linear.LSyntax
import Linear.Lexer
}

%name parser
%error { parserError }

%tokentype { Lexeme }

%monad { P }
%lexer { lexer } { L _ EOF }

%left ';'
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
  : S ';' S         { sL2 $1 $3 $ CompoundStm $1 $3 }
  | id ':=' E       { sL2 $1 $3 $ AssignStm (retrieveID $1) $3 }
  | print '(' L ')' { sL2 $1 $4 $ PrintStm (reverse $3) }

E :: { LExp }
  : id              { sL1 $1 $ Id (retrieveID $1) }
  | num             { sL1 $1 $ Num (retrieveNUM $1) }
  | E '+' E         { sL2 $1 $3 $ Plus $1 $3 }
  | E '-' E         { sL2 $1 $3 $ Minus $1 $3 }
  | E '*' E         { sL2 $1 $3 $ Times $1 $3 }
  | E '/' E         { sL2 $1 $3 $ Div $1 $3 }
  | '(' S ',' E ')' { sL2 $1 $5 $ ESeq $2 $4 }

L :: { [LExp] }
  : E               { [$1] }
  | L ',' E         { $3 : $1 } -- left recursion

{

parserError :: Lexeme -> P a
parserError (L span tk) = failP $ concat [srcFile span, ":", show $ srcSRow span, ":", show $ srcSCol span, ": parser error: token = ", show tk]


retrieveID :: Lexeme -> LId
retrieveID (L loc (ID id)) = L loc id

retrieveNUM :: Lexeme -> Int
retrieveNUM (L _ (NUM n)) = n
}