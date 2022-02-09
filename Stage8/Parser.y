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
    int            { TokenIntVal $$ }
    str            { TokenStrVal $$ }
    id             { TokenVar $$ }
    '+'            { TokenPlus }
    '-'            { TokenMinus }
    '*'            { TokenTimes }
    '/'            { TokenDiv }
    '%'            { TokenMod }
    '('            { TokenLParen }
    ')'            { TokenRParen }
    '['            { TokenLBox }
    ']'            { TokenRBox }
    '{'            { TokenLCurly }
    '}'            { TokenRCurly }
    '&'            { TokenAddr }
    '='            { TokenAssign }
    '<'            { TokenLt }
    '>'            { TokenGt }
    '<='           { TokenLtE }
    '>='           { TokenGtE }
    '!='           { TokenNE }
    '=='           { TokenEq }
    '.'            { TokenDot }
    BEGIN          { TokenBegin }
    END            { TokenEnd }
    READ           { TokenRead }
    WRITE          { TokenWrite }
    ALLOC          { TokenAlloc }
    FREE           { TokenFree }
    NEW            { TokenNew }
    DELETE         { TokenDelete }
    INITIALIZE     { TokenInitialize }
    IF             { TokenIf }
    THEN           { TokenThen }
    ELSE           { TokenElse }
    ENDIF          { TokenEndif }
    WHILE          { TokenWhile }
    DO             { TokenDo }
    ENDWHILE       { TokenEndwhile }
    BREAK          { TokenBreak }
    CONTINUE       { TokenCont }
    INT            { TokenInt }
    STR            { TokenStr }
    DECL           { TokenDecl }
    ENDDECL        { TokenEndDecl }
    TYPE           { TokenType }
    ENDTYPE        { TokenEndType }
    CLASS          { TokenClass }
    ENDCLASS       { TokenEndClass }
    SELF           { TokenSelf }
    EXTENDS        { TokenExtends }
    MAIN           { TokenMain }
    RETURN         { TokenReturn }
    NULL           { TokenNull }
    ','            { TokenComma }
    ';'            { TokenStmtEnd }

%nonassoc '==' '!=' '>' '<' '>=' '<='
%left '+' '-'
%left '*' '/' '%'

%%

Program : TypeDefBlock ClassDefBlock GDeclBlock FDefBlock MainBlock      { ($2, $3, $4, $5) }
        | TypeDefBlock ClassDefBlock GDeclBlock MainBlock                { ($2, $3, [], $4) }

{- TYPE DEFINITIONS GRAMMAR -}
TypeDefBlock : TYPE TypeDefList ENDTYPE    {% saveTypeTable $2 }
             | TYPE ENDTYPE                {% saveTypeTable [] }
             | {- empty -}                 {% saveTypeTable [] }

TypeDefList : TypeDefList TypeDef          { $1 ++ [$2] }
            | TypeDef                      { [$1] }

TypeDef : id '{' FieldDeclList '}'         { ($1, $3) }

FieldDeclList : FieldDeclList FieldDecl    { $1 ++ [$2] }
              | FieldDecl                  { [$1] }

FieldDecl : Type BaseVar ';'               { ($1, $2) }

Type : INT                                 { "int" }
     | STR                                 { "str" }
     | id                                  { $1 }

{- CLASS DEFINITIONS GRAMMAR -}
ClassDefBlock : CLASS ClassDefList ENDCLASS               { $2 }
              | CLASS ENDCLASS                            { [] }
              | {- empty -}                               { [] }

ClassDefList : ClassDefList ClassDef                      { $1 ++ [$2] }
             | ClassDef                                   { [$1] }

ClassDef : CName '{' ClassDecl MethodDefns '}'            {% endCurClass >> return ($1, $4) }

CName : id                                                {% saveCurClass $1 Nothing }
      | id EXTENDS id                                     {% saveCurClass $1 (Just $3) }

ClassDecl : DECL FieldDeclList MethodDecl ENDDECL         {% insertCTable $2 $3 }

MethodDecl : MethodDecl MDecl                             { $1 ++ [$2] }
           | MDecl                                        { [$1] }

MDecl : Type id '(' ParamList ')' ';'                     { ($1, F $2 $4) }
      | Type '*' id '(' ParamList ')' ';'                 { ($1, PF $3 $5) }

MethodDefns : MethodDefns FDef                            { $1 ++ [$2] }
            | FDef                                        { [$1] }

{- GLOBAL DECLARATIONS GRAMMAR -}

GDeclBlock : DECL GDeclList ENDDECL     {% saveGTable $2 }
           | DECL ENDDECL               { 4096 }
           | {- empty -}                { 4096 }

GDeclList : GDeclList GDecl             { $1 ++ [$2] }
          | GDecl                       { [$1] }

GDecl : Type GVarList ';'               { ($1, $2) }

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

FDefBlock : FDefBlock FDef                                           { $1 ++ [$2] }
          | FDef                                                     { [$1] }

FDef : FType FName '(' ParamList ')' '{' LDeclBlock Routine '}'      {% fnTypeCheck ($1, $2, $4, $7, $8) } 
FType: Type                                                          { $1 }
     | Type '*'                                                      { $1 ++ "*" }
FName: id                                                            {% saveCurFn $1 }


LDeclBlock : DECL LDeclList ENDDECL     {% saveLTable $2 }
           | DECL ENDDECL               {% saveLTable [] }
           | {- empty -}                {% saveLTable [] }

LDeclList : LDeclList LDecl             { $1 ++ [$2] }
          | LDecl                       { [$1] }

LDecl : Type LVarList ';'               { ($1, $2) }

LVarList : LVarList ',' BaseVar         { $1 ++ [$3] }
         | BaseVar                      { [$1] }

Routine : BEGIN Slist Retstmt END       { NodeConn $2 $3 } 
        | BEGIN Retstmt END             { $2 }

Retstmt : RETURN RVal ';'               {% let (t, v) = $2 in (retTypeCheck t >> return (NodeReturn v)) }

RVal : Variable                         {% varType $1 >>= \t -> return (t, $1) }
     | FnCall                           {% fnType $1 >>= \t -> return (t, $1) }
     | String                           { ("str", $1) }
     | E2                               { ("int", $1) }
     | '&' Variable                     {% varType $2 >>= \t -> return (t++"*", NodeRef $2) }
     | NULL                             { ("null", LeafNull) }

Slist : Slist Stmt                      { NodeConn $1 $2 }
      | Stmt                            { $1 }

Stmt : Variable '=' RVal ';'                         {% let (t, v) = $3 in (assignTypeCheck $1 t >> return (NodeAssign $1 v)) } 
     | IF '(' B ')' THEN Slist ENDIF ';'             { NodeIf $3 $6 }
     | IF '(' B ')' THEN Slist ELSE Slist ENDIF ';'  { NodeIfElse $3 $6 $8 }
     | WHILE '(' B ')' DO Slist ENDWHILE ';'         { NodeWhile $3 $6 }
     | BREAK ';'                                     { NodeBreak }
     | CONTINUE ';'                                  { NodeCont }
     | FnCall ';'                                    { $1 }
     | Variable '=' ALLOC '(' ')' ';'                {% varType $1 >>= userTypeSize >>= \s -> return (NodeAlloc $1 s) } 
     | Variable '=' NEW '(' ')' ';'                  {% varType $1 >>= classTypeSize >>= \s -> return (NodeAlloc $1 s) } 
     | Variable '=' NEW '(' id ')' ';'               {% varType $1 >>= classTypeSize >>= \s -> return (NodeAlloc $1 s) } 

E2 : E '+' E                            { NodeArmc '+' $1 $3 }
   | E '-' E                            { NodeArmc '-' $1 $3 }
   | E '*' E                            { NodeArmc '*' $1 $3 }
   | E '/' E                            { NodeArmc '/' $1 $3 }
   | E '%' E                            { NodeArmc '%' $1 $3 }
   | '(' E2 ')'                         { $2 }
   | int                                { LeafValInt $1 }

E : E2                                  { $1 }
  | FnCall                              {% intCheck $1 }
  | Variable                            {% intCheck $1 }

FnCall: id '(' ArgList ')'              {% fnCallTypeCheck $1 $3 >>= \(fl, p) -> return (LeafFn fl p) }
      | READ '(' Variable ')'           { NodeRead $3 }
      | WRITE '(' RVal ')'              { let (t, v) = $3 in NodeWrite v } 
      | FREE '(' RVal ')'               {% let (t, v) = $3 in (userTypeSize t >> return (NodeFree v)) } 
      | DELETE '(' RVal ')'             {% let (t, v) = $3 in (classTypeSize t >> return (NodeFree v)) } 
      | INITIALIZE '(' ')'              { NodeInitialize }
      | Inst DotField '('ArgList')'     {% dotFnCallCheck $1 $2 $4 >>= \(fl, p, s) -> return (LeafMtd ($1, s) fl p) }

Self : SELF                             {% selfCheck >> return "self"}
Inst : Self                             { $1 }
     | id                               { $1 }

ArgList : ArgList ',' RVal              { $1 ++ [$3] }
        | RVal                          { [$1] }
        | {- empty -}                   { [] }

B : RVal '<' RVal                       { NodeBool "<" (snd $1) (snd $3) }
  | RVal '>' RVal                       { NodeBool ">" (snd $1) (snd $3) }
  | RVal '<=' RVal                      { NodeBool "<=" (snd $1) (snd $3) } 
  | RVal '>=' RVal                      { NodeBool ">=" (snd $1) (snd $3) } 
  | RVal '==' RVal                      { NodeBool "==" (snd $1) (snd $3) }
  | RVal '!=' RVal                      { NodeBool "!=" (snd $1) (snd $3) }

Variable : id                           {% symCheck (isUnit) $1 >> return (LeafVar $1 Simple) }
         | id '[' E ']'                 {% symCheck (isArr) $1 >> return (LeafVar $1 (Index $3)) }
         | id '['E']' '['E']'           {% symCheck (isArr2) $1 >> return (LeafVar $1 (Index2D $3 $6)) }
         | '*' id                       {% symCheck (isPtr) $2 >> return (LeafVar $2 Deref) }
         | Inst DotField                {% dotAccCheck $1 $2 >>= \di -> return (LeafVar $1 (Dot di)) }

DotField : DotField '.' id              { $1 ++ [$3] }
         | '.' id                       { [$2] }

String : str                            { LeafValStr $1 }


{- MAIN FUNCTION GRAMMAR -}

MainBlock : INT Main '(' ')' '{' LDeclBlock Routine '}'      { ($6, $7) } 
Main: MAIN                                                   {% saveMainFn }

{

parseError t = error $ "Parse error: " ++ show t

parseTokens tokenStream = (cTable, cfDecls, gSymFull, sp, fDecl++[("int", "main",[],mainSym, mainTree)])
  where
    ( (cfDecls, sp, fDecl, (mainSym, mainTree)),
      ParserState {gTable = gTable, cTable = cTable}
      ) = runState (parse tokenStream) startState
    gSymFull = Map.insertWith (error "Redeclaration of main function") "main" (Func "int" [] "main") gTable

type Program =
  ( 
    [(String, [FDef])],         -- class def block
    Int,                        -- sp
    [FDef],                     -- fn defns
    (LSymbolTable, SyntaxTree)  -- main fn
  )

}