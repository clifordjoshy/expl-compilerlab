{
module Grammar where
import Tokens
import Tree
}

%name parseCalc
%tokentype { Token }
%error { parseError }

%token
    int { TokenInt $$ }
    '+' { TokenPlus }
    '-' { TokenMinus }
    '*' { TokenTimes }
    '/' { TokenDiv }
    '(' { TokenLParen }
    ')' { TokenRParen }

%left '+' '-'
%left '*' '/'

%%

Exp : Exp '+' Exp            { Node '+' $1 $3 }
    | Exp '-' Exp            { Node '-' $1 $3 }
    | Exp '*' Exp            { Node '*' $1 $3 }
    | Exp '/' Exp            { Node '/' $1 $3 }
    | '(' Exp ')'            { $2 }
    | int                    { $1 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

type Exp = Tree
}