{
module Grammar where
import Tokens
import SyntaxTree
import qualified Data.Map as Map
import SymbolTable (SymbolBase(..))
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
    '%'       { TokenMod }
    '('       { TokenLParen }
    ')'       { TokenRParen }
    '['       { TokenLBox }
    ']'       { TokenRBox }
    '&'       { TokenAddr }
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
%left '*' '/' '%'

%%

Main: Declarations Program      { ($1, $2) }

Program : BEGIN Slist END       { $2 } 
        | BEGIN END             { NodeEmpty }

Slist : Slist Stmt              { NodeConn $1 $2 }
      | Stmt                    { $1 }

Stmt : READ '(' Variable ')' ';'                           { NodeStmt "Read" $3 }
     | WRITE '(' E ')' ';'                                 { NodeStmt "Write" $3 } 
     | WRITE '(' String ')' ';'                            { NodeStmt "Write" $3 } 
     | Variable '=' E ';'                                  { NodeAssign $1 $3 }
     | Variable '=' String ';'                             { NodeAssign $1 $3}
     | Variable '=' '&' Variable ';'                       { NodeAssign $1 (NodeRef $4) }
     | IF '(' B ')' THEN Slist ENDIF ';'                   { NodeIf $3 $6 }
     | IF '(' B ')' THEN Slist ELSE Slist ENDIF ';'        { NodeIfElse $3 $6 $8 }
     | WHILE '(' B ')' DO Slist ENDWHILE ';'               { NodeWhile $3 $6 }
     | BREAK ';'                                           { NodeBreak }
     | CONTINUE ';'                                        { NodeCont }

E : E '+' E            { NodeArmc '+' $1 $3 }
  | E '-' E            { NodeArmc '-' $1 $3 }
  | E '*' E            { NodeArmc '*' $1 $3 }
  | E '/' E            { NodeArmc '/' $1 $3 }
  | E '%' E            { NodeArmc '%' $1 $3 }
  | '(' E ')'          { $2 }
  | int                { LeafValInt $1 }
  | Variable           { $1 }

B : E '<' E            { NodeBool "<" $1 $3 }
  | E '>' E            { NodeBool ">" $1 $3 }
  | E '<=' E           { NodeBool "<=" $1 $3 } 
  | E '>=' E           { NodeBool ">=" $1 $3 } 
  | E '!=' E           { NodeBool "!=" $1 $3 } 
  | E '==' E           { NodeBool "==" $1 $3 } 

Variable : var                          { LeafVar $1 Simple}
         | var '[' E ']'                { LeafVar $1 (Index $3) }
         | var '['E']' '['E']'          { LeafVar $1 (Index2D $3 $6) }
         | '*' var                      { LeafVar $2 Deref }

String : str                            { LeafValStr $1 }

Declarations : DECL DeclList ENDDECL    { $2 }
             | DECL ENDDECL             { [] }

DeclList : DeclList Decl                { $1 ++ [$2] }
         | Decl                         { [$1] }

Decl : Type VarList ';'                 { ($1, $2) }

Type : INT                              { "int" }
     | STR                              { "str" }

VarList : VarList ',' Var               { $1 ++ [$3] }
        | Var                           { [$1] }
        
Var : var                               { U $1 }
    | var '[' int ']'                   { A $1 $3 }
    | var '['int']' '['int']'           { A2 $1 $3 $6 }
    | '*' var                           { P $2 }

{

parseError :: [Token] -> a
parseError t = error $"Parse error: " ++ show t

typeError :: SyntaxTree -> a
typeError t = error $ "Type Error: " ++ show t 

type Var = SymbolBase
}