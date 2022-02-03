module ParserState where

import Control.Monad.State (MonadState (get, put), State, unless)
import Data.Bifunctor (second)
import qualified Data.Map as Map
import SymbolTable
import SyntaxTree
import TypeTable

-- Global Symbol Table, Current Function, Local Symbol Table
type ParserState = (TypeTable, GSymbolTable, Symbol, GSymbolTable)

-- Unit "" 0 is a temp junk value
startState = (Map.empty, Map.empty, Unit "" 0, Map.empty)

-- STATE UPDATE FUNCTIONS

-- | Saves and type checks the type table
saveTypeTable :: [(String, [(String, SymbolBase)])] -> State ParserState ()
saveTypeTable tlist = do
  (_, s1, s2, s3) <- get
  let baseToTuple (t, v) = case v of
        U n -> (t, n)
        P n -> (t ++ "*", n)
        _ -> error "Not a valid type child"
  let tList = map (second (map baseToTuple)) tlist
  let tTable = genTypeTable tList
  put (tTable, s1, s2, s3)
  return ()

-- | Saves global symbol table and returns stack pointer
saveGTable :: [(String, [SymbolBase])] -> State ParserState Int
saveGTable decls = do
  (tt, _, start1, start2) <- get
  let (gSymTable, spInc) = genGSymbolTable decls
  let gSymTableV = verifyGSymTable gSymTable tt
  put (tt, gSymTableV, start1, start2)
  -- stack pointer should be pointing to the last used location
  return $ spInc - 1

-- | Saves and returns local symbol table
saveLTable :: [(String, [SymbolBase])] -> State ParserState LSymbolTable
saveLTable decls = do
  (tt, gSymTable, cFn, _) <- get
  let (Func _ args _) = cFn
      lSymTable = genLSymbolTable decls
      argSymTable = genArgSymbolTable args
      localSymbols = Map.unionWith (error "Args and local variables have same name") lSymTable argSymTable
      lSymG = Map.map (uncurry Unit) localSymbols
      mergedSymTable = Map.union lSymG gSymTable
  put (tt, gSymTable, cFn, mergedSymTable)
  return $ verifyLSymTable localSymbols tt

-- | Saves current function as symbol
saveCurFn :: String -> State ParserState String
saveCurFn name = do
  (tt, gSymTable, _, lSymTable) <- get
  let curFn =
        ( case Map.lookup name gSymTable of
            Just f -> f
            Nothing -> error $ "Function " ++ name ++ " not declared."
        )
  put (tt, gSymTable, curFn, lSymTable)
  return name

saveMainFn :: State ParserState String
saveMainFn = do
  (tt, gSymTable, _, lSymTable) <- get
  let curFn = Func "int" [] ""
  put (tt, gSymTable, curFn, lSymTable)
  return "main"

-- TYPE CHECK FUNCTIONS

type FDef = (String, String, [String], LSymbolTable, SyntaxTree)

-- Typechecks and returns given fn defn with modified params
fnTypeCheck :: (String, String, [(String, SymbolBase)], LSymbolTable, SyntaxTree) -> State ParserState FDef
fnTypeCheck (t, name, params, lSym, tree) = do
  (_, _, cFn, _) <- get
  let (Func td pd _) = cFn
      paramToTuple (t, v) = case v of
        U n -> (t, n)
        P n -> (t ++ "*", n)
        _ -> error "Not a valid param"
  let pc = map paramToTuple params
  let params2 = map snd pc
  if t == td && pc == pd
    then return (t, name, params2, lSym, tree)
    else error "Function definition does not match declaration"

tEq :: String -> String -> Bool
tEq t1 t2 = case (t1, t2) of
  ("null", t) -> isUserType t
  (t, "null") -> isUserType t
  (ta, tb) -> ta == tb
  where
    isUserType "int" = False
    isUserType "str" = False
    isUserType _ = True

-- Typechecks and returns given fn defn with modified params
fnCallTypeCheck :: String -> [(String, SyntaxTree)] -> State ParserState [SyntaxTree]
fnCallTypeCheck name params = do
  (_, gSymT, _, _) <- get
  case Map.lookup name gSymT of
    Just (Func _ p2 _) ->
      if (length params == length p2) && and (zipWith (\(t1, _) (t2, _) -> tEq t1 t2) params p2)
        then return (map snd params)
        else error $ "Function call doesnt match definition: " ++ name ++ show (map fst params)
    _ -> error $ "Function call is invalid: " ++ name

retTypeCheck :: String -> State ParserState ()
retTypeCheck t = do
  (_, _, cFn, _) <- get
  let (Func td _ _) = cFn
  if t `tEq` td then return () else error $ "Function returns " ++ t ++ " instead of " ++ td

assignTypeCheck :: SyntaxTree -> String -> State ParserState ()
assignTypeCheck n tc = do
  t <- varType n
  if t `tEq` tc then return () else error $ "Cannot assign " ++ tc ++ " to " ++ t

userTypeCheck :: String -> State ParserState ()
userTypeCheck tName = do
  (tt, _, _, _) <- get
  case Map.lookup tName tt of
    Just _ -> return ()
    Nothing -> error $ "Cannot do heap operations on " ++ tName

dotSymCheck :: String -> [String] -> State ParserState ()
dotSymCheck symName dots = do
  (tTable, _, _, symTab) <- get
  case Map.lookup symName symTab of
    Just s -> unless (null (resolveDotType tTable (getSymbolType s) dots)) (return ())
    Nothing -> error $ "Variable does not exist : " ++ symName

intCheck :: SyntaxTree -> State ParserState SyntaxTree
intCheck n = do
  (_, _, _, symTab) <- get
  if isInteger symTab n then return n else error $ "Integer value was expected on right side of assignment: " ++ show n

symCheck :: (Symbol -> Bool) -> String -> State ParserState ()
symCheck isSym n = do
  (_, _, _, symTab) <- get
  case Map.lookup n symTab of
    Just s -> if isSym s then return () else error $ "Illegal variable access for " ++ n
    Nothing -> error $ "Variable does not exist : " ++ n

varType :: SyntaxTree -> State ParserState String
varType n = do
  (tt, _, _, symTab) <- get
  return $ getVarType symTab tt n

fnType :: SyntaxTree -> State ParserState String
fnType n = do
  (_, _, _, symTab) <- get
  return $ getFnType symTab n
