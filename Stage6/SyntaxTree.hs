module SyntaxTree (SyntaxTree (..), VarResolve (..), prettyPrint, isInteger, getVarType, getFnType, findVarType) where

import qualified Data.Map as Map
import Data.Tree (Tree (Node), drawTree)
import SymbolTable
import TypeTable

data VarResolve
  = Simple
  | Deref
  | Index SyntaxTree
  | Index2D SyntaxTree SyntaxTree
  | Dot [String]
  deriving (Show)

data SyntaxTree
  = LeafVar String VarResolve
  | LeafFn String [SyntaxTree]
  | LeafValInt Int
  | LeafValStr String
  | LeafNull
  | NodeRead SyntaxTree
  | NodeWrite SyntaxTree
  | NodeInitialize
  | NodeAlloc SyntaxTree
  | NodeFree SyntaxTree
  | NodeArmc Char SyntaxTree SyntaxTree
  | NodeBool String SyntaxTree SyntaxTree
  | NodeAssign SyntaxTree SyntaxTree
  | NodeConn SyntaxTree SyntaxTree
  | NodeIf SyntaxTree SyntaxTree
  | NodeIfElse SyntaxTree SyntaxTree SyntaxTree
  | NodeWhile SyntaxTree SyntaxTree
  | NodeRef SyntaxTree
  | NodeBreak
  | NodeCont
  | NodeReturn SyntaxTree
  deriving (Show)

-- | Returns if a given constructor evaluates to an integer value
isInteger :: GSymbolTable -> SyntaxTree -> Bool
isInteger st (LeafVar var Deref) = getSymbolType (st Map.! var) == "int*"
isInteger st (LeafVar var _) = getSymbolType (st Map.! var) == "int"
isInteger st (LeafFn name _) = getSymbolType (st Map.! name) == "int"
isInteger _ LeafValInt {} = True
isInteger _ NodeArmc {} = True
isInteger _ _ = False

-- Used in ParserState. Takes merged sym table
getVarType :: GSymbolTable -> TypeTable -> SyntaxTree -> String
getVarType st _ (LeafVar var Deref) = init $ getSymbolType (st Map.! var) -- Remove "*" from the end
getVarType st tt (LeafVar var (Dot dotList)) = resolveDotType tt (getSymbolType (st Map.! var)) dotList
getVarType st _ (LeafVar var _) = getSymbolType (st Map.! var)
getVarType _ _ _ = error "Not a variable"

-- Takes lsym and gsym separately
findVarType :: GSymbolTable -> LSymbolTable -> TypeTable -> SyntaxTree -> String
findVarType gst lst _ (LeafVar var Deref) = init $ findSymbolType var lst gst -- Remove "*" from the end
findVarType gst lst tt (LeafVar var (Dot dotList)) = resolveDotType tt (findSymbolType var lst gst) dotList
findVarType gst lst _ (LeafVar var _) = findSymbolType var lst gst
findVarType _ _ _ _ = error "Not a variable. (shouldn't be thrown)"

getFnType :: GSymbolTable -> SyntaxTree -> String
getFnType st (LeafFn name _) = getSymbolType (st Map.! name)
getFnType st (NodeRead _) = "int"
getFnType st (NodeWrite _) = "int"
getFnType st NodeInitialize = "int"
getFnType st (NodeFree _) = "int"
getFnType st (NodeAlloc _) = "int"
getFnType _ _ = error "Not a function"

toDataTree :: SyntaxTree -> Tree String
toDataTree t = case t of
  NodeArmc c l r -> Node ("Arithmetic " ++ [c]) [toDataTree l, toDataTree r]
  NodeBool c l r -> Node ("Boolean " ++ c) [toDataTree l, toDataTree r]
  NodeConn l r -> Node "NodeConn" [toDataTree l, toDataTree r]
  NodeAssign l r -> Node "Assign" [toDataTree l, toDataTree r]
  NodeIf cond bl -> Node "If" [toDataTree cond, toDataTree bl]
  NodeIfElse cond bl1 bl2 -> Node "If Else" [toDataTree cond, toDataTree bl1, toDataTree bl2]
  NodeWhile cond bl -> Node "While" [toDataTree cond, toDataTree bl]
  NodeRead t -> Node "Read" [toDataTree t]
  NodeWrite t -> Node "Write" [toDataTree t]
  NodeAlloc t -> Node "Alloc" [toDataTree t]
  NodeFree t -> Node "Free" [toDataTree t]
  _ -> Node (show t) []

prettyPrint :: SyntaxTree -> String
prettyPrint t = drawTree $ toDataTree t