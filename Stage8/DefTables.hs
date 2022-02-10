module DefTables where

import Data.List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import SymbolTable

type Field = (String, String) -- TypeName, VarName

type TypeTable = Map.Map String [Field] -- Name -> TypeInfo

data CTableEntry = Class
  { size :: Int,
    st :: GSymbolTable, -- Symbols = Func and Unit only
    parent :: Maybe String
  }
  deriving (Show)

type ClassTable = Map.Map String CTableEntry

type FnTableList = [(String, [String])] -- className, [fnLabels]

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
getTypeSize tTable tName = length tEntry
  where
    tEntry = case Map.lookup tName tTable of
      Just e -> e
      Nothing -> error $ "Not a valid user type: " ++ tName

-- | Checks if a given array of types all fit into the type table and class table
areValidTypes :: TypeTable -> ClassTable -> [String] -> Bool
areValidTypes tTable cTable = all tCheck
  where
    validTypes = Map.keys tTable ++ Map.keys cTable
    tCheck "int" = True
    tCheck "str" = True
    tCheck t =
      elem (if last t == '*' then init t else t) validTypes
        || error ("Type '" ++ t ++ "' not recognized." ++ show validTypes)

-- | Generates and type-checks a type table with passed definitions
genTypeTable :: [(String, [Field])] -> TypeTable
genTypeTable tList =
  if isValid
    then tTable
    else error "Types cannot contain more than 8 attributes."
  where
    tTable = Map.fromListWith nameError tList
    nameError = error "Non-unique type name"
    fieldsVerify :: [Field] -> Bool
    fieldsVerify fields = length fields <= 8 && areValidTypes tTable Map.empty (map fst fields)
    isValid = all (fieldsVerify . snd) tList

-- Verifies if the passed global symbol table has valid types
verifyGSymTable :: GSymbolTable -> TypeTable -> ClassTable -> GSymbolTable
verifyGSymTable gTable tTable cTable =
  if isValid
    then gTable
    else error "This error shouldn't be thrown #2"
  where
    symbolTypes = map getSymbolType $ Map.elems gTable
    isValid = areValidTypes tTable cTable symbolTypes

verifyLSymTable :: LSymbolTable -> TypeTable -> LSymbolTable
verifyLSymTable lTable tTable =
  if isValid
    then lTable
    else error "This error shouldn't be thrown #3"
  where
    symbolTypes = map fst $ Map.elems lTable
    isValid = areValidTypes tTable Map.empty symbolTypes

-- checks if the first class is an ancestor of the second class
isAncestor :: ClassTable -> String -> String -> Bool
isAncestor ct a b
  | a == b = True
  | otherwise = case parent (ct Map.! b) of
    Just p -> isAncestor ct a p
    Nothing -> False

ftlHelper :: String -> ClassTable -> Map.Map String [(String, String)] -> (Map.Map String [(String, String)], ClassTable)
ftlHelper cName toPcs pcsd = (pcsd3, toPcs3)
  where
    Class {parent = cParent, st = cst} = toPcs Map.! cName
    metList :: [(String, String)]
    metList = Map.toList $ Map.mapMaybe (\sym -> if isFunc sym then Just (getFuncLabel sym) else Nothing) cst

    orderMetList :: [(String, String)] -> [(String, String)]
    orderMetList pMets = sortOn (\(n, _) -> fromMaybe 8 (findIndex ((n ==) . fst) pMets)) metList

    (pcsd2, toPcs2) = case cParent of
      Nothing -> (Map.insert cName metList pcsd, Map.delete cName toPcs)
      Just pName -> case Map.lookup pName pcsd of
        Just pMets -> (Map.insert cName (orderMetList pMets) pcsd, Map.delete cName toPcs)
        Nothing -> ftlHelper pName toPcs pcsd

    (pcsd3, toPcs3)
      | Map.null toPcs2 = (pcsd2, toPcs2)
      | otherwise = ftlHelper (fst $ Map.findMin toPcs2) toPcs2 pcsd2

-- Generates function table list such that all the inherited method decls come first in the order of declarations in parent
genFtl :: ClassTable -> FnTableList
genFtl ct = Map.toList $ Map.map (map snd) mListi
  where
    (mListi, _) = ftlHelper (fst $ Map.findMin ct) ct Map.empty