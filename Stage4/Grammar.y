{
module Grammar where
import Tokens
import SyntaxTree
import qualified Data.Map as Map
}

%name parseCalc
%tokentype { Token }
%error { parseError }

%token
    int       { TokenIntVal $$ }
    str       { TokenStrVal $$ }
    var       { TokenVar $$ }
    '+'       { TokenPlus }
    '-'       { TokenMinus }
    '*'       { TokenTimes }
    '/'       { TokenDiv }
    '('       { TokenLParen }
    ')'       { TokenRParen }
    '='       { TokenAssign }
    '<'       { TokenLt }
    '>'       { TokenGt }
    '<='      { TokenLtE }
    '>='      { TokenGtE }
    '!='      { TokenNE }
    '=='      { TokenEq }
    BEGIN     { TokenBegin }
    END       { TokenEnd }
    READ      { TokenRead }
    WRITE     { TokenWrite }
    IF        { TokenIf }
    THEN      { TokenThen }
    ELSE      { TokenElse }
    ENDIF     { TokenEndif }
    WHILE     { TokenWhile }
    DO        { TokenDo }
    ENDWHILE  { TokenEndwhile }
    BREAK     { TokenBreak }
    CONTINUE  { TokenCont }
    INT       { TokenInt }
    STR       { TokenStr }
    DECL      { TokenDecl }
    ENDDECL   { TokenEndDecl }
    ','       { TokenComma }
    ';'       { TokenStmtEnd }

%nonassoc '==' '!=' '>' '<' '>=' '<='
%left '+' '-'
%left '*' '/'

%%

Main: Declarations Program      { ($1, $2) }

Program : BEGIN Slist END       { $2 } 
        | BEGIN END             { NodeEmpty }

Slist : Slist Stmt              { NodeConn $1 $2 }
      | Stmt                    { $1 }

Stmt : READ '(' Variable ')' ';'                           { NodeStmt "Read" $3 }
     | WRITE '(' E ')' ';'                                 { if isInteger $3 then (NodeStmt "Write" $3) else typeError $3 } 
     | WRITE '(' String ')' ';'                            { NodeStmt "Write" $3 } 
     | Variable '=' E ';'                                  { if isInteger $3 then (NodeAssign $1 $3) else typeError $3 }
     | Variable '=' String ';'                             { NodeAssign $1 $3}
     | IF '(' E ')' THEN Slist ENDIF ';'                   { if isBoolean $3 then (NodeIf $3 $6) else typeError $3 }
     | IF '(' E ')' THEN Slist ELSE Slist ENDIF ';'        { if isBoolean $3 then (NodeIfElse $3 $6 $8) else typeError $3 }
     | WHILE '(' E ')' DO Slist ENDWHILE ';'               { if isBoolean $3 then (NodeWhile $3 $6) else typeError $3 }
     | BREAK ';'                                           { NodeBreak }
     | CONTINUE ';'                                        { NodeCont }

E : E '+' E            { NodeArmc '+' $1 $3 }
  | E '-' E            { NodeArmc '-' $1 $3 }
  | E '*' E            { NodeArmc '*' $1 $3 }
  | E '/' E            { NodeArmc '/' $1 $3 }
  | E '<' E            { NodeBool "<" $1 $3 }
  | E '>' E            { NodeBool ">" $1 $3 }
  | E '<=' E           { NodeBool "<=" $1 $3 } 
  | E '>=' E           { NodeBool ">=" $1 $3 } 
  | E '!=' E           { NodeBool "!=" $1 $3 } 
  | E '==' E           { NodeBool "==" $1 $3 } 
  | '(' E ')'          { $2 }
  | int                { LeafValInt $1 }
  | Variable           { $1 }

Variable : var         { LeafVar $1 }
String : str           { LeafValStr $1 }

Declarations : DECL DeclList ENDDECL    { $2 }
             | DECL ENDDECL             { [] }

DeclList : DeclList Decl                { $1 ++ [$2] }
         | Decl                         { [$1] }

Decl : Type VarList ';'                 { ($1, $2) }

Type : INT                              { "int" }
     | STR                              { "str" }

VarList : VarList ',' var               { $1 ++ [$3] }
        | var                           { [$1] }


{

parseError :: [Token] -> a
parseError t = error $"Parse error: " ++ show t

typeError :: SyntaxTree -> a
typeError t = error $ "Type Error: " ++ show t 

}