module SyntaxTree (SyntaxTree (..), VarResolve (..), prettyPrint, isInteger, isString, isValidPtr) where

import qualified Data.Map as Map
import Data.Tree (Tree (Node), drawTree)
import SymbolTable (SymbolTable, getType)

data VarResolve = Simple | Deref | Index SyntaxTree | Index2D SyntaxTree SyntaxTree deriving (Show)

data SyntaxTree
  = LeafVar String VarResolve
  | LeafValInt Int
  | LeafValStr String
  | LeafFn String [SyntaxTree]
  | NodeStmt String SyntaxTree
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
  | NodeEmpty
  deriving (Show)

-- | Returns if a given constructor evaluates to an integer value
isInteger :: SymbolTable -> SyntaxTree -> Bool
isInteger st (LeafVar var _) = getType (st Map.! var) == "int"
isInteger _ LeafValInt {} = True
isInteger _ NodeArmc {} = True
isInteger _ _ = False

-- | Returns if a given constructor evaluates to a string value
isString :: SymbolTable -> SyntaxTree -> Bool
isString _ LeafValStr {} = True
isString st (LeafVar var _) = getType (st Map.! var) == "str"
isString _ _ = False

-- | type -> SymbolTable -> node -> isValid
-- | Returns if a given SyntaxTree evaluates to a pointer of given type
isValidPtr :: String -> SymbolTable -> SyntaxTree -> Bool
isValidPtr t st (NodeRef (LeafVar var _)) = getType (st Map.! var) == t
isValidPtr t st (LeafVar var _) = getType (st Map.! var) == (t ++ "ptr")
isValidPtr _ _ _ = False

toDataTree :: SyntaxTree -> Tree String
toDataTree t = case t of
  NodeStmt c s -> Node c [toDataTree s]
  NodeArmc c l r -> Node ("Arithmetic " ++ [c]) [toDataTree l, toDataTree r]
  NodeBool c l r -> Node ("Boolean " ++ c) [toDataTree l, toDataTree r]
  NodeConn l r -> Node "NodeConn" [toDataTree l, toDataTree r]
  NodeAssign l r -> Node "Assign" [toDataTree l, toDataTree r]
  NodeIf cond bl -> Node "If" [toDataTree cond, toDataTree bl]
  NodeIfElse cond bl1 bl2 -> Node "If Else" [toDataTree cond, toDataTree bl1, toDataTree bl2]
  NodeWhile cond bl -> Node "While" [toDataTree cond, toDataTree bl]
  NodeBreak -> Node "Break" []
  NodeCont -> Node "Continue" []
  NodeEmpty -> Node "Empty" []
  _ -> Node (show t) []

prettyPrint :: SyntaxTree -> String
prettyPrint t = drawTree $ toDataTree t