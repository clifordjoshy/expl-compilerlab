module SymbolTable where

import qualified Data.Map as Map

-- | Type used in the Grammar file for making the variable list
data SymbolBase = U String | A String Int | A2 String Int Int | P String

data Symbol
  = Unit String Int -- Type, Addr
  | Arr String Int Int -- Type, Size, Addr
  | Arr2 String Int Int Int -- Type, Size1, Size2, Addr
  | Ptr String Int -- Type, Addr
  deriving (Show)

type SymbolTable = Map.Map String Symbol

-- | Returns the type of a given symbol
getType :: Symbol -> String
getType (Unit t _) = t
getType (Arr t _ _) = t
getType (Arr2 t _ _ _) = t
getType (Ptr t _) = t ++ "ptr"

-- | (type, [base]) -> startAddress -> ([(name, symbol)], nextAddr)
-- | Converts a list of SymbolBase to a list of Symbol (map style)
baseToSymbolList :: (String, [SymbolBase]) -> Int -> ([(String, Symbol)], Int)
baseToSymbolList (_, []) addr = ([], addr)
baseToSymbolList (t, b : bs) addr = (sym : bsList, cAddr + size)
  where
    (bsList, cAddr) = baseToSymbolList (t, bs) addr
    size = case b of
      U _ -> 1
      A _ n -> n
      A2 _ n1 n2 -> n1 * n2
      P _ -> 1
    sym = case b of
      U name -> (name, Unit t cAddr)
      A name s -> (name, Arr t s cAddr)
      A2 name s1 s2 -> (name, Arr2 t s1 s2 cAddr)
      P name -> (name, Ptr t cAddr)

-- | Array of decls ("type": [vars]) -> (SymbolTable, freeAddr)
genSymbolTable :: [(String, [SymbolBase])] -> (SymbolTable, Int)
genSymbolTable [] = (Map.empty, 4096)
genSymbolTable (d : ds) = (Map.unionWith nameError map1 map2, addrEnd)
  where
    (map1, addr) = genSymbolTable ds
    (symbolList, addrEnd) = baseToSymbolList d addr
    map2 = Map.fromListWith nameError symbolList
    nameError = error "Non-unique variable declaration"
