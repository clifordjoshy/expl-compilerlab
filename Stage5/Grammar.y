{
module Grammar where
import Tokens
import SyntaxTree
import qualified Data.Map as Map
import SymbolTable
}

%name parseCalc
%tokentype { Token }
%error { parseError }

%token
    int       { TokenIntVal $$ }
    str       { TokenStrVal $$ }
    id        { TokenVar $$ }
    '+'       { TokenPlus }
    '-'       { TokenMinus }
    '*'       { TokenTimes }
    '/'       { TokenDiv }
    '%'       { TokenMod }
    '('       { TokenLParen }
    ')'       { TokenRParen }
    '['       { TokenLBox }
    ']'       { TokenRBox }
    '{'       { TokenLCurly }
    '}'       { TokenRCurly }
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
    MAIN      { TokenMain }
    ','       { TokenComma }
    ';'       { TokenStmtEnd }

%nonassoc '==' '!=' '>' '<' '>=' '<='
%left '+' '-'
%left '*' '/' '%'

%%

Program : GDeclBlock FDefBlock MainBlock      { (Just $1, Just $2, $3) }
        | GDeclBlock MainBlock                { (Just $1, Nothing, $2) }
        | MainBlock                           { (Nothing, Nothing, $1) }


{- GLOBAL DECLARATIONS GRAMMAR -}

GDeclBlock : DECL GDeclList ENDDECL     { $2 }
           | DECL ENDDECL               { [] }

GDeclList : GDeclList GDecl             { $1 ++ [$2] }
          | GDecl                       { [$1] }

GDecl : Type GVarList ';'               { ($1, $2) }

Type : INT                              { "int" }
     | STR                              { "str" }

GVarList : GVarList ',' GDecVar         { $1 ++ [$3] }
         | GDecVar                      { [$1] }
        
GDecVar : id                            { U $1 }
        | id '[' int ']'                { A $1 $3 }
        | id '['int']' '['int']'        { A2 $1 $3 $6 }
        | '*' id                        { P $2 }
        | id '(' ParamList ')'          { F $1 $3 }

ParamList : ParamList ',' Param         { $1 ++ [$3] }
          | Param                       { [$1] }
          | {- empty -}                 { [] }

Param : Type id                         { ($1, $2) }

{- FUNCTION DEFINITION GRAMMAR -}

FDefBlock : FDefBlock FDef                                       { $1 ++ [$2] }
          | FDef                                                 { [$1] }

FDef : Type id '(' ParamList ')' '{' LDeclBlock Routine '}'      { ($1, $2, $4, $7, $8) } 

LDeclBlock : DECL LDeclList ENDDECL     { $2 }
           | DECL ENDDECL               { [] }

LDeclList : LDeclList LDecl             { $1 ++ [$2] }
          | LDecl                       { [$1] }

LDecl : Type LVarList ';'               { ($1, $2) }

LVarList : LVarList ',' id              { $1 ++ [$3] }
         | id                           { [$1] }


Routine : BEGIN Slist END               { $2 } 
        | BEGIN END                     { NodeEmpty }

Slist : Slist Stmt                      { NodeConn $1 $2 }
      | Stmt                            { $1 }

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
     {-todo make E a valid statement-}

E : E '+' E                             { NodeArmc '+' $1 $3 }
  | E '-' E                             { NodeArmc '-' $1 $3 }
  | E '*' E                             { NodeArmc '*' $1 $3 }
  | E '/' E                             { NodeArmc '/' $1 $3 }
  | E '%' E                             { NodeArmc '%' $1 $3 }
  | '(' E ')'                           { $2 }
  | id '(' ArgList ')'                  { LeafFn $1 $3}
  | int                                 { LeafValInt $1 }
  | Variable                            { $1 }

ArgList : ArgList ',' E                 { $1 ++ [$3] }
        | E                             { [$1] }
        | {- empty -}                   { [] }

B : E '<' E                             { NodeBool "<" $1 $3 }
  | E '>' E                             { NodeBool ">" $1 $3 }
  | E '<=' E                            { NodeBool "<=" $1 $3 } 
  | E '>=' E                            { NodeBool ">=" $1 $3 } 
  | E '!=' E                            { NodeBool "!=" $1 $3 } 
  | E '==' E                            { NodeBool "==" $1 $3 } 

Variable : id                           { LeafVar $1 Simple}
         | id '[' E ']'                 { LeafVar $1 (Index $3) }
         | id '['E']' '['E']'           { LeafVar $1 (Index2D $3 $6) }
         | '*' id                       { LeafVar $2 Deref }

String : str                            { LeafValStr $1 }


{- MAIN FUNCTION GRAMMAR -}

MainBlock : INT MAIN '(' ')' '{' LDeclBlock Routine '}'      { ($6, $7) } 


{

parseError :: [Token] -> a
parseError t = error $"Parse error: " ++ show t

typeError :: SyntaxTree -> a
typeError t = error $ "Type Error: " ++ show t 

type Program = (
  Maybe [(String, [SymbolBase])], 
  Maybe [(String, String, [(String, String)], [(String, String)], SyntaxTree)],
  [([(String, String)], SyntaxTree)]
  )

}