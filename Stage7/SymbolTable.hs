{-# LANGUAGE ScopedTypeVariables #-}

module SymbolTable where

import Data.List
import qualified Data.Map as Map

-- | Type used in the Grammar file for making the variable list
data SymbolBase
  = U String
  | A String Int
  | A2 String Int Int
  | P String
  | F String [(String, SymbolBase)]
  | PF String [(String, SymbolBase)]
  deriving (Show, Eq)

data Symbol
  = Unit String Int -- Type, Addr
  | Arr String Int Int -- Type, Size, Addr
  | Arr2 String Int Int Int -- Type, Size1, Size2, Addr
  | Func String [(String, String)] String -- Type, Params, Label
  deriving (Show)

type GSymbolTable = Map.Map String Symbol

type LSymbolTable = Map.Map String (String, Int) -- Name -> (Type, Local Addr)

-- | Returns the type of a given symbol
getSymbolType :: Symbol -> String
getSymbolType (Unit t _) = t
getSymbolType (Arr t _ _) = t
getSymbolType (Arr2 t _ _ _) = t
getSymbolType (Func t _ _) = t

isUnit :: Symbol -> Bool
isUnit Unit {} = True
isUnit _ = False

isPtr :: Symbol -> Bool
isPtr (Unit t _) = last t == '*'
isPtr _ = False

isArr :: Symbol -> Bool
isArr Arr {} = True
isArr _ = False

isArr2 :: Symbol -> Bool
isArr2 Arr2 {} = True
isArr2 _ = False

getSymbolAddress :: Symbol -> Int
getSymbolAddress (Unit _ a) = a
getSymbolAddress (Arr _ _ a) = a
getSymbolAddress (Arr2 _ _ _ a) = a
getSymbolAddress _ = error "No address for function"

findSymbolType :: String -> LSymbolTable -> GSymbolTable -> String
findSymbolType name lst gst = case Map.lookup name lst of
  Just (t, _) -> t
  Nothing -> getSymbolType (gst Map.! name)

-- | Base declarations -> typetable -> startAddress -> mapFunction -> errorMsg -> (SymbolTable, Size)
symbolTableHelper :: forall tVal. [(String, [SymbolBase])] -> Int -> (String -> SymbolBase -> Int -> (String, tVal)) -> String -> (Map.Map String tVal, Int)
symbolTableHelper decls sa toTableEntry eMsg = (Map.fromListWith (error eMsg) symList, size)
  where
    getBaseSize :: SymbolBase -> String -> Int
    getBaseSize b t = case b of
      U _ -> 1
      A _ n -> n
      A2 _ n1 n2 -> n1 * n2
      P _ -> 1
      _ -> 0

    getLineSyms :: Int -> (String, [SymbolBase]) -> (Int, [(String, tVal)])
    getLineSyms addrInit (t, vars) = mapAccumL (\a b -> (a + getBaseSize b t, toTableEntry t b a)) addrInit vars

    (size, symList) = let (s, l) = mapAccumL getLineSyms sa decls in (s, concat l)

toGEntry :: String -> SymbolBase -> Int -> (String, Symbol)
toGEntry t v a = case v of
  U name -> (name, Unit t a)
  A name s -> (name, Arr t s a)
  A2 name s1 s2 -> (name, Arr2 t s1 s2 a)
  P name -> (name, Unit (t ++ "*") a)
  F name params -> (name, Func t (map paramToTuple params) name)
  PF name params -> (name, Func (t ++ "*") (map paramToTuple params) name)
  where
    paramToTuple (t, v) = case v of
      U n -> (t, n)
      P n -> (t ++ "*", n)
      _ -> error "Not a valid param"

-- | Array of decls ("type": [SymbolBase]) -> (GSymbolTable, size)
genGSymbolTable :: [(String, [SymbolBase])] -> (GSymbolTable, Int)
genGSymbolTable decls = symbolTableHelper decls 4096 toGEntry eMsg
  where
    eMsg = "Non-unique global declaration"

genClassSymbolTable :: [(String, SymbolBase)] -> GSymbolTable
genClassSymbolTable mems = fst $ symbolTableHelper memsMapped 0 toGEntry eMsg
  where
    memsMapped = map (\(a, b) -> (a, [b])) mems
    eMsg = "Non-unique members in class"

-- | Array of decls ("type": [unit/ptr])  -> LSymbolTable
genLSymbolTable :: [(String, [SymbolBase])] -> LSymbolTable
genLSymbolTable decls = fst $ symbolTableHelper decls 0 toTableEntry eMsg
  where
    toTableEntry t v a = case v of
      U n -> (n, (t, a))
      P n -> (n, (t ++ "*", a))
      _ -> error "Local variables can only be pointers or simple"
    eMsg = "Non-unique local variable declaration"

-- | The args will have space BP - 4 and upwards
-- | [(type, name)] -> symboltable
genArgSymbolTable :: [(String, String)] -> LSymbolTable
genArgSymbolTable args = Map.fromListWith nameError symbolList
  where
    startAddr = -3 - length args
    -- (_, symbolList) = mapAccumL (\a ((t, n), s) -> (a + s, (n, (t, a)))) startAddr $ zip args argSizes
    symbolList = [(n, (t, i)) | ((t, n), i) <- zip args [startAddr ..]]
    nameError = error "Non-unique argument declaration"
