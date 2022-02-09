{-# OPTIONS_GHC -Wno-missing-fields #-}

module ParserState where

import Control.Monad.State (MonadState (get, put), State)
import Data.Bifunctor (second)
import qualified Data.Map as Map
import DefTables
import SymbolTable
import SyntaxTree

data ParserState = ParserState
  { tTable :: TypeTable,
    curClass :: Maybe (String, Maybe String),
    cTable :: ClassTable,
    gTable :: GSymbolTable,
    curFn :: Symbol,
    lTable :: GSymbolTable
  }

startState =
  ParserState
    { tTable = Map.empty,
      curClass = Nothing,
      cTable = Map.empty,
      gTable = Map.empty,
      curFn = Unit "" 0, -- junk value
      lTable = Map.empty
    }

-- STATE UPDATE FUNCTIONS

-- | Saves and type checks the type table
saveTypeTable :: [(String, [(String, SymbolBase)])] -> State ParserState ()
saveTypeTable tlist = do
  s <- get
  let baseToTuple (t, v) = case v of
        U n -> (t, n)
        P n -> (t ++ "*", n)
        _ -> error "Not a valid type child"
  let tList = map (second (map baseToTuple)) tlist
  let tTable = genTypeTable tList
  put s {tTable = tTable}
  return ()

-- | Sets current class name to the curClass Type
saveCurClass :: String -> Maybe String -> State ParserState String
saveCurClass cName parent = do
  s <- get
  put s {curClass = Just (cName, parent)}
  return cName

-- | Saves, typechecks and returns size of current class
insertCTable :: [(String, SymbolBase)] -> [(String, SymbolBase)] -> State ParserState ()
insertCTable attrs mets = do
  s@ParserState {tTable = tt, cTable = ct, curClass = curClass} <- get
  let Just (cName, parent) = curClass

      Class {size = pSize, st = pSyms} = case parent of
        Just pName -> case Map.lookup pName ct of
          Just e -> e
          Nothing -> error $ "Cannot find parent class " ++ pName
        Nothing -> Class {size = 0, st = Map.empty}

      (cSymTable, cSize) = genClassSymbolTable cName (attrs ++ mets) pSize pSyms

      -- add current class to class table (temporarily) (for recursive decl)
      ct2 = Map.insertWith (error "Non-unique class name") cName Class {} ct
      cSymTableV = verifyGSymTable cSymTable tt ct2

  put s {cTable = Map.insert cName Class {size = cSize, st = cSymTable, parent = parent} ct}
  return ()

-- | Pushes current class to the class table
endCurClass :: State ParserState ()
endCurClass = do
  s <- get
  put s {curClass = Nothing}

-- | Saves global symbol table and returns stack pointer
saveGTable :: [(String, [SymbolBase])] -> State ParserState Int
saveGTable decls = do
  s@ParserState {tTable = tt, cTable = ct} <- get
  let (gSymTable, spInc) = genGSymbolTable decls
  let gSymTableV = verifyGSymTable gSymTable tt ct
  put s {gTable = gSymTableV}
  -- stack pointer should be pointing to the last used location
  return $ spInc - 1

-- | Saves and returns local symbol table. handles class as well.
saveLTable :: [(String, [SymbolBase])] -> State ParserState LSymbolTable
saveLTable decls = do
  s@ParserState {tTable = tt, gTable = gSymTable, curFn = cFn, curClass = cClass} <- get
  let (Func _ argsB _) = cFn
      (ttCheck, args) = case cClass of
        Nothing -> (tt, argsB)
        Just (cName, _) -> (Map.insert cName [] tt, (cName, "self") : argsB)
      lSymTable = genLSymbolTable decls
      argSymTable = genArgSymbolTable args
      localSymbols = Map.unionWith (error "Args and local variables have same name") lSymTable argSymTable
      lSymG = Map.map (uncurry Unit) localSymbols
      mergedSymTable = Map.union lSymG gSymTable
  put s {lTable = mergedSymTable}
  -- add the current class as a temp type (for the implicit self argument)
  return $ verifyLSymTable localSymbols ttCheck

-- | Saves current function as symbol
saveCurFn :: String -> State ParserState String
saveCurFn name = do
  s@ParserState {cTable = ct, gTable = gSymTable, curClass = curClass} <- get
  let symTab = case curClass of
        Nothing -> gSymTable
        Just (cName, _) -> st $ ct Map.! cName
  let curFn = case Map.lookup name symTab of
        Just f -> f
        Nothing -> error $ "Function " ++ name ++ " not declared."

  put s {curFn = curFn}
  return name

saveMainFn :: State ParserState String
saveMainFn = do
  s <- get
  let curFn = Func "int" [] ""
  put s {curFn = curFn}
  return "main"

-- TYPE CHECK FUNCTIONS

type FDef = (String, String, [String], LSymbolTable, SyntaxTree)

-- Typechecks and returns given fn defn with modified params
fnTypeCheck :: (String, String, [(String, SymbolBase)], LSymbolTable, SyntaxTree) -> State ParserState FDef
fnTypeCheck (t, name, params, lSym, tree) = do
  ParserState {curFn = cFn} <- get
  let (Func td pd _) = cFn
      paramToTuple (t, v) = case v of
        U n -> (t, n)
        P n -> (t ++ "*", n)
        _ -> error "Not a valid param"
  let pc = map paramToTuple params
  let params2 = map snd pc
  if t == td && pc == pd
    then return (t, name, params2, lSym, tree)
    else error $ "Function definition does not match declaration: " ++ name

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
fnCallTypeCheck :: String -> [(String, SyntaxTree)] -> State ParserState (String, [SyntaxTree])
fnCallTypeCheck name params = do
  ParserState {gTable = gSymT} <- get
  case Map.lookup name gSymT of
    Just (Func _ p2 fLabel) ->
      if (length params == length p2) && and (zipWith (\(t1, _) (t2, _) -> tEq t1 t2) params p2)
        then return (fLabel, map snd params)
        else error $ "Function call doesnt match definition: " ++ name ++ show (map fst params)
    _ -> error $ "Function call is invalid: " ++ name

-- Typechecks and returns given fn defn with modified params
dotFnCallCheck :: String -> [String] -> [(String, SyntaxTree)] -> State ParserState (String, [SyntaxTree], Maybe Symbol)
dotFnCallCheck insName dots params = do
  ParserState {cTable = cTable, curClass = curClass, lTable = symTab} <- get
  let sym = case Map.lookup insName symTab of
        Just s -> s
        Nothing -> error $ "Cannot find instance " ++ insName
      sClass = getSymbolType sym
      cst1 = case Map.lookup sClass cTable of
        Just c -> st c
        Nothing -> error $ sClass ++ " is not a class."

      (cst2, subSym) = case length dots of
        1 -> (cst1, Nothing)
        2 ->
          if insName == "self"
            then case Map.lookup (head dots) cst1 of
              Just s -> (st $ cTable Map.! getSymbolType s, Just s)
              Nothing -> error $ sClass ++ " does not contain child " ++ head dots
            else error "Cannot access child from outside class scope."
        _ -> error "Cannot access child from outside class scope."

      mName = last dots

  case Map.lookup mName cst2 of
    Just (Func _ p2 fLabel) ->
      if (length params == length p2) && and (zipWith (\(t1, _) (t2, _) -> tEq t1 t2) params p2)
        then return (fLabel, map snd params, subSym)
        else error $ "Method call doesnt match definition: " ++ sClass ++ "." ++ mName ++ show (map fst params)
    _ -> error $ "Method call is invalid: " ++ sClass ++ "." ++ mName

-- Typechecks and returns modified index dotfields
dotAccCheck :: String -> [String] -> State ParserState [Int]
dotAccCheck "self" (attrName : dots) = do
  ParserState {tTable = tTable, curClass = curClass, cTable = cTable} <- get
  let Just (cName, _) = curClass
      symTab = st $ cTable Map.! cName
  case Map.lookup attrName symTab of
    Just s -> return $ getSymbolAddress s : dotStrToIndex tTable (getSymbolType s) dots
    Nothing -> error $ "Cannot find attribute " ++ attrName ++ " of " ++ cName
dotAccCheck symName dots = do
  ParserState {tTable = tTable, lTable = symTab} <- get
  case Map.lookup symName symTab of
    Just s -> return $ dotStrToIndex tTable (getSymbolType s) dots
    Nothing -> error $ "Variable does not exist : " ++ symName

retTypeCheck :: String -> State ParserState ()
retTypeCheck t = do
  ParserState {curFn = cFn} <- get
  let (Func td _ _) = cFn
  if t `tEq` td then return () else error $ "Function returns " ++ t ++ " instead of " ++ td

assignTypeCheck :: SyntaxTree -> String -> State ParserState ()
assignTypeCheck n tc = do
  t <- varType n
  if t `tEq` tc then return () else error $ "Cannot assign " ++ tc ++ " to " ++ t

userTypeSize :: String -> State ParserState Int
userTypeSize tName = do
  ParserState {tTable = tt} <- get
  case Map.lookup tName tt of
    Just _ -> return $ getTypeSize tt tName
    Nothing -> error $ "Cannot do alloc/free on " ++ tName

classTypeSize :: String -> State ParserState Int
classTypeSize cName = do
  ParserState {cTable = ct} <- get
  let cSize = case Map.lookup cName ct of
        Just ce -> size ce
        Nothing -> error $ "Not a valid class type: " ++ cName
  case Map.lookup cName ct of
    Just _ -> return cSize
    Nothing -> error $ "Cannot do new/delete on " ++ cName

intCheck :: SyntaxTree -> State ParserState SyntaxTree
intCheck n = do
  ParserState {tTable = tt, lTable = lSymT, cTable = ct, curClass = curClass} <- get
  let classSymT = case curClass of
        Just (cName, _) -> st $ ct Map.! cName
        Nothing -> Map.empty
      symTab = case n of
        LeafVar "self" _ -> classSymT
        _ -> lSymT
  if isInteger symTab tt ct n then return n else error $ "Integer value was expected: " ++ show n

symCheck :: (Symbol -> Bool) -> String -> State ParserState ()
symCheck isSym n = do
  ParserState {lTable = symTab} <- get
  case Map.lookup n symTab of
    Just s -> if isSym s then return () else error $ "Illegal variable access for " ++ n
    Nothing -> error $ "Variable does not exist : " ++ n

selfCheck :: State ParserState ()
selfCheck = do
  ParserState {curClass = curClass} <- get
  case curClass of
    Just _ -> return ()
    Nothing -> error "self can only be used within a method."

varType :: SyntaxTree -> State ParserState String
varType n = do
  ParserState {tTable = tt, lTable = lSymT, curClass = curClass, cTable = ct} <- get
  let classSymT = case curClass of
        Just (cName, _) -> st $ ct Map.! cName
        Nothing -> Map.empty
      (LeafVar varName _) = n
      symTab = if varName == "self" then classSymT else lSymT
  return $ getVarType symTab tt n

fnType :: SyntaxTree -> State ParserState String
fnType n = do
  ParserState {lTable = symTab, cTable = ct} <- get
  return $ getFnType symTab ct n
