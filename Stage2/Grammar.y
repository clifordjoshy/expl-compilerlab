{
module Grammar where
import Tokens
import SyntaxTree
}

%name parseCalc
%tokentype { Token }
%error { parseError }

%token
    int { TokenInt $$ }
    var { TokenVar $$}
    '+' { TokenPlus }
    '-' { TokenMinus }
    '*' { TokenTimes }
    '/' { TokenDiv }
    '(' { TokenLParen }
    ')' { TokenRParen }
    '=' { TokenEquals }
    BEGIN {TokenBegin}
    END {TokenEnd}
    READ {TokenRead}
    WRITE {TokenWrite}
    ';' {TokenStmtEnd}

%left '+' '-'
%left '*' '/'

%%

Program : BEGIN Slist END {$2} 
        | BEGIN END {NodeEmpty}

Slist : Slist Stmt {NodeConn $1 $2}
      | Stmt {$1}

Stmt : READ '(' var ')' ';'   {NodeStmt "Read" $3}
     | WRITE '(' E ')' ';'    {NodeStmt "Write" $3} 
     | var '=' E ';'          {NodeEq $1 $3}

E : E '+' E            { NodeArmc '+' $1 $3 }
  | E '-' E            { NodeArmc '-' $1 $3 }
  | E '*' E            { NodeArmc '*' $1 $3 }
  | E '/' E            { NodeArmc '/' $1 $3 }
  | '(' E ')'          { $2 }
  | int                { $1 }
  | var                { $1 }
  

{

parseError :: [Token] -> a
parseError t = error $"Parse error: "++show t

type E = SyntaxTree
type Stmt = SyntaxTree
type Slist = SyntaxTree
type Program = SyntaxTree

}