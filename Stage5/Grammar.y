{
module Grammar(parseTokens) where
import Tokens
import SyntaxTree
import qualified Data.Map as Map
import SymbolTable
import Control.Monad.State
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

Retstmt : RETURN Variable ';'           {% varType $2 >>= retTypeCheck >> return (NodeReturn $2) }
        | RETURN E ';'                  {% retTypeCheck "int" >> return (NodeReturn $2) }
        | RETURN String ';'             {% retTypeCheck "str" >> return (NodeReturn $2) }

Slist : Slist Stmt                      { NodeConn $1 $2 }
      | Stmt                            { $1 }

Stmt : READ '(' Variable ')' ';'                           { NodeStmt "Read" $3 }
     | WRITE '(' E ')' ';'                                 { NodeStmt "Write" $3 } 
     | WRITE '(' String ')' ';'                            { NodeStmt "Write" $3 } 
     | Variable '=' FnCall ';'                             {% fnType $3 >>= (\t -> assignTypeCheck $1 t) >> return (NodeAssign $1 $3) } 
     | Variable '=' E ';'                                  {% assignTypeCheck $1 "int" >> return (NodeAssign $1 $3) }
     | Variable '=' String ';'                             {% assignTypeCheck $1 "str" >> return (NodeAssign $1 $3) }
     | Variable '=' '&' Variable ';'                       {% varType $4 >>= (\t -> assignTypeCheck $1 (t++"ptr")) >> return (NodeAssign $1 (NodeRef $4)) }
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
  | int                                 { LeafValInt $1 }
  | FnCall                              {% intCheck $1 }
  | Variable                            {% intCheck $1 }

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

Variable : id                           { LeafVar $1 Simple}
         | id '[' E ']'                 { LeafVar $1 (Index $3) }
         | id '['E']' '['E']'           { LeafVar $1 (Index2D $3 $6) }
         | '*' id                       { LeafVar $2 Deref }

String : str                            { LeafValStr $1 }


{- MAIN FUNCTION GRAMMAR -}

MainBlock : INT Main '(' ')' '{' LDeclBlock Routine '}'      { ($6, $7) } 
Main: MAIN                                                   {% saveMainFn }

{
type ParserState = (GSymbolTable, Symbol, GSymbolTable)

-- Unit "" 0 is a temp junk value
startState = (Map.empty, Unit "" 0, Map.empty)

parseError t = error $ "Parse error: " ++ show t

typeError t = error $ "Type Error: " ++ show t

-- STATE UPDATE FUNCTIONS
-- Saves and returns global symbol table
saveGTable :: [(String, [SymbolBase])] -> State ParserState Int
saveGTable decls = do
  (_, start1, start2) <- get
  let (gSymTable, sp, _) = genGSymbolTable decls
  put (gSymTable, start1, start2)
  return sp

-- Saves and returns local symbol table
saveLTable :: [(String, [SymbolBase])] -> State ParserState LSymbolTable
saveLTable decls = do
  (gSymTable, cFn, _) <- get
  let (lSymTable, _) = genLSymbolTable decls
      lSymG = Map.map (uncurry Unit) lSymTable
      (Func _ args _) = cFn
      (argSymTable, _) = genLSymbolTable (fmap (\(a, b) -> (a, [b])) args)
      argSymG = Map.map (uncurry Unit) argSymTable
      localSymbols = Map.unionWith (error "Args and local variables have same name") lSymG argSymG
      mergedSymTable = Map.union localSymbols gSymTable
  put (gSymTable, cFn, mergedSymTable)
  return lSymTable

-- Saves current function as symbol
saveCurFn :: String -> State ParserState String
saveCurFn name = do
  (gSymTable, _, lSymTable) <- get
  let curFn =
        ( case Map.lookup name gSymTable of
            Just f -> f
            Nothing -> error $ "Function " ++ name ++ " not declared."
        )
  put (gSymTable, curFn, lSymTable)
  return name

saveMainFn :: State ParserState String
saveMainFn = do
  (gSymTable, _, lSymTable) <- get
  let curFn = Func "int" [] ""
  put (gSymTable, curFn, lSymTable)
  return "main"

-- TYPE CHECK FUNCTIONS
type FDef = (String, String, [(String, SymbolBase)], LSymbolTable, SyntaxTree)

-- Typechecks and returns given fn defn
fnTypeCheck :: FDef -> State ParserState FDef
fnTypeCheck a@(t, name, params, _, _) = do
  (_, cFn, _) <- get
  let (Func td pd _) = cFn
  if t == td && params == pd then return a else error "Function definition does not match declaration"

retTypeCheck :: String -> State ParserState ()
retTypeCheck t = do
  (_, cFn, _) <- get
  let (Func td _ _) = cFn
  if t == td then return () else error $ "Function returns " ++ t ++ " instead of " ++ td

assignTypeCheck :: SyntaxTree -> String -> State ParserState ()
assignTypeCheck n tc = do
  t <- varType n
  if t == tc then return () else error $ "Cannot assign " ++ tc ++ " to " ++ t

intCheck :: SyntaxTree -> State ParserState SyntaxTree
intCheck n = do
  (_, _, symTab) <- get
  if isInteger symTab n then return n else error "Integer value was expected"

varType :: SyntaxTree -> State ParserState String
varType n = do
  (_, _, symTab) <- get
  return $ getVarType symTab n

fnType :: SyntaxTree -> State ParserState String
fnType n = do
  (_, _, symTab) <- get
  return $ getFnType symTab n

parseTokens tokenStream = (gSymTable, sp, fDecl, main)
  where
    ((sp, fDecl, main), (gSymTable, _, _)) = runState (parse tokenStream) startState

type Program =
  ( Int,
    [FDef],
    [([(String, String)], SyntaxTree)]
  )

}