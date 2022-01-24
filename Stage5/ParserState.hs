module ParserState where

import Control.Monad.State
import qualified Data.Map as Map
import SymbolTable
import SyntaxTree

type ParserState = (GSymbolTable, Symbol, GSymbolTable)

-- Unit "" 0 is a temp junk value
startState = (Map.empty, Unit "" 0, Map.empty)

-- STATE UPDATE FUNCTIONS
-- Saves and returns global symbol table
saveGTable :: [(String, [SymbolBase])] -> State ParserState Int
saveGTable decls = do
  (_, start1, start2) <- get
  let (gSymTable, spInc, _) = genGSymbolTable decls
  put (gSymTable, start1, start2)
  return $ spInc - 1

-- Saves and returns local symbol table
saveLTable :: [(String, [SymbolBase])] -> State ParserState LSymbolTable
saveLTable decls = do
  (gSymTable, cFn, _) <- get
  let (Func _ args _) = cFn
      lSymTable = genLSymbolTable decls
      argSymTable = genArgSymbolTable args
      localSymbols = Map.unionWith (error "Args and local variables have same name") lSymTable argSymTable
      lSymG = Map.map (uncurry Unit) localSymbols
      mergedSymTable = Map.union lSymG gSymTable

  put (gSymTable, cFn, mergedSymTable)
  return localSymbols

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

type FDef = (String, String, [String], LSymbolTable, SyntaxTree)

-- Typechecks and returns given fn defn with modified params
fnTypeCheck :: (String, String, [(String, SymbolBase)], LSymbolTable, SyntaxTree) -> State ParserState FDef
fnTypeCheck (t, name, params, lSym, tree) = do
  (_, cFn, _) <- get
  let (Func td pd _) = cFn
      paramToTuple (t, v) = case v of
        U n -> (t, n)
        P n -> (t ++ "ptr", n)
        _ -> error "Not a valid param"
  let pc = map paramToTuple params
  let params2 = map snd pc
  if t == td && pc == pd
    then return (t, name, params2, lSym, tree)
    else error "Function definition does not match declaration"

-- Typechecks and returns given fn defn with modified params
fnCallTypeCheck :: String -> [(String, SyntaxTree)] -> State ParserState [SyntaxTree]
fnCallTypeCheck name params = do
  (gSymT, _, _) <- get
  case Map.lookup name gSymT of
    Just (Func _ p2 _) ->
      if map fst params == map fst p2
        then return (map snd params)
        else error $ "Function call doesnt match definition: " ++ name ++ show (map fst params) ++ show ()
    _ -> error $ "Function call is invalid: " ++ name

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
  if isInteger symTab n then return n else error $ "Integer value was expected on right side of assignment: " ++ show n

symCheck :: (Symbol -> Bool) -> String -> State ParserState ()
symCheck isSym n = do
  (_, _, symTab) <- get
  case Map.lookup n symTab of
    Just s -> if isSym s then return () else error $ "Illegal variable access for " ++ n
    Nothing -> error $ "Variable does not exist : " ++ n

varType :: SyntaxTree -> State ParserState String
varType n = do
  (_, _, symTab) <- get
  return $ getVarType symTab n

fnType :: SyntaxTree -> State ParserState String
fnType n = do
  (_, _, symTab) <- get
  return $ getFnType symTab n
