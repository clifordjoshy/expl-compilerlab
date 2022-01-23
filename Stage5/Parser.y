{
module Parser(parseTokens) where
import Tokens
import Control.Monad.State
import ParserState
import qualified Data.Map as Map
import SymbolTable
import SyntaxTree
}

%name parse
%tokentype { Token }
%error { parseError }
%monad { State ParserState }

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
    RETURN    { TokenReturn }
    ','       { TokenComma }
    ';'       { TokenStmtEnd }

%nonassoc '==' '!=' '>' '<' '>=' '<='
%left '+' '-'
%left '*' '/' '%'

%nonassoc LOWER
%nonassoc HIGHER

%%

Program : GDeclBlock FDefBlock MainBlock      { ($1, $2, $3) }
        | GDeclBlock MainBlock                { ($1, [], $2) }
        | MainBlock                           { (4096, [], $1) }


{- GLOBAL DECLARATIONS GRAMMAR -}

GDeclBlock : DECL GDeclList ENDDECL     {% saveGTable $2 }
           | DECL ENDDECL               { 4096 }

GDeclList : GDeclList GDecl             { $1 ++ [$2] }
          | GDecl                       { [$1] }

GDecl : Type GVarList ';'               { ($1, $2) }

Type : INT                              { "int" }
     | STR                              { "str" }

GVarList : GVarList ',' GDec            { $1 ++ [$3] }
         | GDec                         { [$1] }

GDec: BaseVar                           { $1 }
    | id '(' ParamList ')'              { F $1 $3 }
    | '*' id '(' ParamList ')'          { PF $2 $4 }
    | id '[' int ']'                    { A $1 $3 }
    | id '['int']' '['int']'            { A2 $1 $3 $6 }

BaseVar : id                            { U $1 }
        | '*' id                        { P $2 }

ParamList : ParamList ',' Param         { $1 ++ [$3] }
          | Param                       { [$1] }
          | {- empty -}                 { [] }

Param : Type BaseVar                    { ($1, $2) }

{- FUNCTION DEFINITION GRAMMAR -}

FDefBlock : FDefBlock FDef                                       { $1 ++ [$2] }
          | FDef                                                 { [$1] }

FDef : FType FName '(' ParamList ')' '{' LDeclBlock Routine '}'      {% fnTypeCheck ($1, $2, $4, $7, $8) } 
FType: Type                                                          { $1 }
     | Type'*'                                                       { $1 ++ "ptr" }
FName: id                                                            {% saveCurFn $1 }


LDeclBlock : DECL LDeclList ENDDECL     {% saveLTable $2 }
           | DECL ENDDECL               { Map.empty }

LDeclList : LDeclList LDecl             { $1 ++ [$2] }
          | LDecl                       { [$1] }

LDecl : Type LVarList ';'               { ($1, $2) }

LVarList : LVarList ',' BaseVar         { $1 ++ [$3] }
         | BaseVar                      { [$1] }

Routine : BEGIN Slist Retstmt END       { NodeConn $2 $3 } 
        | BEGIN Retstmt END             { $2 }

Retstmt : RETURN RVal ';'               {% let (t, v) = $2 in (retTypeCheck t >> return (NodeReturn v)) }

RVal : Variable       %prec HIGHER      {% varType $1 >>= \t -> return (t, $1) }
     | FnCall         %prec HIGHER      {% fnType $1 >>= \t -> return (t, $1) }
     | String                           { ("str", $1) }
     | E                                { ("int", $1) }
     | '&' Variable                     {% varType $2 >>= \t -> return (t++"ptr", NodeRef $2) }

Slist : Slist Stmt                      { NodeConn $1 $2 }
      | Stmt                            { $1 }

Stmt : READ '(' Variable ')' ';'                           { NodeStmt "Read" $3 }
     | Variable '=' RVal ';'                               {% let (t, v) = $3 in (assignTypeCheck $1 t >> return (NodeAssign $1 v)) } 
     | WRITE '(' RVal ')' ';'                              { let (t, v) = $3 in NodeStmt "Write" v } 
     | IF '(' B ')' THEN Slist ENDIF ';'                   { NodeIf $3 $6 }
     | IF '(' B ')' THEN Slist ELSE Slist ENDIF ';'        { NodeIfElse $3 $6 $8 }
     | WHILE '(' B ')' DO Slist ENDWHILE ';'               { NodeWhile $3 $6 }
     | BREAK ';'                                           { NodeBreak }
     | CONTINUE ';'                                        { NodeCont }
     | FnCall ';'                                          { $1 }

E : E '+' E                             { NodeArmc '+' $1 $3 }
  | E '-' E                             { NodeArmc '-' $1 $3 }
  | E '*' E                             { NodeArmc '*' $1 $3 }
  | E '/' E                             { NodeArmc '/' $1 $3 }
  | E '%' E                             { NodeArmc '%' $1 $3 }
  | '(' E ')'                           { $2 }
  | int                                 { LeafValInt $1 }
  | FnCall           %prec LOWER        {% intCheck $1 }
  | Variable         %prec LOWER        {% intCheck $1 }

FnCall: id '(' ArgList ')'              { LeafFn $1 $3 }

ArgList : ArgList ',' E                 { $1 ++ [$3] }
        | E                             { [$1] }
        | {- empty -}                   { [] }

B : E '<' E                             { NodeBool "<" $1 $3 }
  | E '>' E                             { NodeBool ">" $1 $3 }
  | E '<=' E                            { NodeBool "<=" $1 $3 } 
  | E '>=' E                            { NodeBool ">=" $1 $3 } 
  | E '!=' E                            { NodeBool "!=" $1 $3 } 
  | E '==' E                            { NodeBool "==" $1 $3 } 

Variable : id                           {% symCheck (isUnit) $1 >> return (LeafVar $1 Simple) }
         | id '[' E ']'                 {% symCheck (isArr) $1 >> return (LeafVar $1 (Index $3)) }
         | id '['E']' '['E']'           {% symCheck (isArr2) $1 >> return (LeafVar $1 (Index2D $3 $6)) }
         | '*' id                       {% symCheck (isPtr) $2 >> return (LeafVar $2 Deref) }

String : str                            { LeafValStr $1 }


{- MAIN FUNCTION GRAMMAR -}

MainBlock : INT Main '(' ')' '{' LDeclBlock Routine '}'      { ($6, $7) } 
Main: MAIN                                                   {% saveMainFn }

{

parseError t = error $ "Parse error: " ++ show t

parseTokens tokenStream = (gSymTable, sp, fDecl, main)
  where
    ((sp, fDecl, main), (gSymTable, _, _)) = runState (parse tokenStream) startState

type Program =
  ( Int,
    [FDef],
    (LSymbolTable, SyntaxTree)
  )

}