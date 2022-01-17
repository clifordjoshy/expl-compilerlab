{
module Grammar where
import Tokens
import SyntaxTree
}

%name parseCalc
%tokentype { Token }
%error { parseError }

%token
    int       { TokenInt $$ }
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
    ';'       { TokenStmtEnd }

%nonassoc '==' '!=' '>' '<' '>=' '<='
%left '+' '-'
%left '*' '/'

%%

Program : BEGIN Slist END       { $2 } 
        | BEGIN END             { NodeEmpty}

Slist : Slist Stmt              { NodeConn $1 $2 }
      | Stmt                    { $1 }

Stmt : READ '(' var ')' ';'                                { NodeStmt "Read" $3 }
     | WRITE '(' E ')' ';'                                 { if isInteger $3 then (NodeStmt "Write" $3) else typeError $3 } 
     | var '=' E ';'                                       { if isInteger $3 then (NodeAssign $1 $3) else typeError $3 }
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
  | int                { $1 }
  | var                { $1 }

{

parseError :: [Token] -> a
parseError t = error $"Parse error: " ++ show t

typeError :: SyntaxTree -> a
typeError t = error $ "Type Error: " ++ show t 

-- | Returns if a given constructor evaluates to an integer value
isInteger :: SyntaxTree -> Bool
isInteger LeafVar {} = True
isInteger LeafVal {} = True
isInteger NodeArmc {} = True
isInteger _ = False

-- | Returns if a given constructor evaluates to a boolean value
isBoolean :: SyntaxTree -> Bool
isBoolean NodeBool {} = True
isBoolean _ = False


type E = SyntaxTree
type Stmt = SyntaxTree
type Slist = SyntaxTree
type Program = SyntaxTree

}