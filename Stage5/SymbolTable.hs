module SymbolTable where

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
  | Func String [(String, SymbolBase)] String -- Type, Params, Label
  deriving (Show)

type GSymbolTable = Map.Map String Symbol

type LSymbolTable = Map.Map String (String, Int) -- Name -> (Type, Local Addr)

-- | Returns the type of a given symbol
getSymbolType :: Symbol -> String
getSymbolType (Unit t _) = t
getSymbolType (Arr t _ _) = t
getSymbolType (Arr2 t _ _ _) = t
getSymbolType (Func t _ _) = t

-- | (type, [base]) -> startAddress -> fnLabels -> ([(name, symbol)], nextAddr, remainingLabels)
-- | Converts a list of SymbolBase to a list of Symbol (map style)
baseToSymbolList :: (String, [SymbolBase]) -> Int -> [String] -> ([(String, Symbol)], Int, [String])
baseToSymbolList (_, []) addr labels = ([], addr, labels)
baseToSymbolList (t, b : bs) addr labels = (sym : bsList, cAddr + size, remainingLabels)
  where
    (bsList, cAddr, labels2) = baseToSymbolList (t, bs) addr labels
    l : remainingLabels = labels2
    size = case b of
      U _ -> 1
      A _ n -> n
      A2 _ n1 n2 -> n1 * n2
      P _ -> 1
      _ -> 0
    sym = case b of
      U name -> (name, Unit t cAddr)
      A name s -> (name, Arr t s cAddr)
      A2 name s1 s2 -> (name, Arr2 t s1 s2 cAddr)
      P name -> (name, Unit (t ++ "ptr") cAddr)
      F name params -> (name, Func t params l)
      PF name params -> (name, Func (t ++ "ptr") params l)

-- | Array of decls ("type": [vars]) -> (GSymbolTable, freeAddr, fnLabels)
genGSymbolTable :: [(String, [SymbolBase])] -> (GSymbolTable, Int, [String])
genGSymbolTable [] = (Map.empty, 4096, ["F" ++ show i | i <- [0, 1 ..]])
genGSymbolTable (d : ds) = (Map.unionWith nameError map1 map2, addrEnd, remainingLabels)
  where
    (map1, addr, labels) = genGSymbolTable ds
    (symbolList, addrEnd, remainingLabels) = baseToSymbolList d addr labels
    map2 = Map.fromListWith nameError symbolList
    nameError = error "Non-unique variable declaration"

-- | Array of decls ("type": [unit/ptr]) -> (LSymbolTable, nextAddr/size)
genLSymbolTable :: [(String, [SymbolBase])] -> (LSymbolTable, Int)
genLSymbolTable [] = (Map.empty, 0)
genLSymbolTable ((t, vars) : ds) = (Map.unionWith nameError map1 map2, addr + length vars)
  where
    (map1, addr) = genLSymbolTable ds
    map2 = Map.fromListWith nameError symbolList
    symbolList = [toTableEntry v i | (v, i) <- zip vars [addr ..]]
    toTableEntry v a = case v of
      U n -> (n, (t, a))
      P n -> (n, (t ++ "ptr", a))
      _ -> error "Local variables can only be pointers or simple"
    nameError = error "Non-unique variable declaration"
