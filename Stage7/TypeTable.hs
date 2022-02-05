module TypeTable where

import Data.List
import qualified Data.Map as Map
import SymbolTable

type Field = (String, String) -- TypeName, VarName

type TypeTable = Map.Map String [Field] -- Name -> TypeInfo

-- | Check if a list of dot access is valid on a given type and returns final type
-- | TypeTable -> startType -> [DotList] -> FinalType
resolveDotType :: TypeTable -> String -> [Int] -> String
resolveDotType tTable = foldl' (\pType cIndex -> fst ((tTable Map.! pType) !! cIndex))

-- | Converts a dot list of strings to field indices
-- | TypeTable -> startType -> stringFields -> indexFields
dotStrToIndex :: TypeTable -> String -> [String] -> [Int]
dotStrToIndex tTable startType fields = intFields
  where
    mapFn :: String -> String -> (String, Int)
    mapFn pType cName = (cType, childIndex)
      where
        pFields = tTable Map.! pType
        (childIndex, (cType, _)) = case findIndex ((cName ==) . snd) pFields of
          Just i -> (i, pFields !! i)
          Nothing -> error $ "Type " ++ pType ++ " does not contain child " ++ cName

    (_, intFields) = mapAccumL mapFn startType fields

-- | Returns the size of a given type string
getTypeSize :: TypeTable -> String -> Int
getTypeSize tTable tName = length $ tTable Map.! tName

-- | Checks if a given array of types all fit into the type table
areValidTypes :: TypeTable -> [String] -> Bool
areValidTypes tTable = all tCheck
  where
    tCheck "int" = True
    tCheck "str" = True
    tCheck t = case Map.lookup (if last t == '*' then init t else t) tTable of
      Just _ -> True
      Nothing -> error $ "Type '" ++ t ++ "' not recognized."

-- | Generates and type-checks a type table with passed definitions
genTypeTable :: [(String, [Field])] -> TypeTable
genTypeTable tList =
  if isValid
    then tTable
    else error "This error shouldn't be thrown #1"
  where
    tTable = Map.fromListWith nameError tList
    nameError = error "Non-unique type name"
    fieldsVerify :: [Field] -> Bool
    fieldsVerify fields = areValidTypes tTable $ map fst fields
    isValid = all (fieldsVerify . snd) tList

-- Verifies if the passed global symbol table has valid types
verifyGSymTable :: GSymbolTable -> TypeTable -> GSymbolTable
verifyGSymTable gTable tTable =
  if isValid
    then gTable
    else error "This error shouldn't be thrown #2"
  where
    symbolTypes = map getSymbolType $ Map.elems gTable
    isValid = areValidTypes tTable symbolTypes

verifyLSymTable :: LSymbolTable -> TypeTable -> LSymbolTable
verifyLSymTable lTable tTable =
  if isValid
    then lTable
    else error "This error shouldn't be thrown #3"
  where
    symbolTypes = map fst $ Map.elems lTable
    isValid = areValidTypes tTable symbolTypes