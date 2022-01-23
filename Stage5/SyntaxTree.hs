module SyntaxTree (SyntaxTree (..), VarResolve (..), prettyPrint, isInteger, getVarType, getFnType) where

import qualified Data.Map as Map
import Data.Tree (Tree (Node), drawTree)
import SymbolTable

data VarResolve = Simple | Deref | Index SyntaxTree | Index2D SyntaxTree SyntaxTree deriving (Show)

data SyntaxTree
  = LeafVar String VarResolve
  | LeafFn String [SyntaxTree]
  | LeafValInt Int
  | LeafValStr String
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
  | NodeReturn SyntaxTree
  deriving (Show)

-- | Returns if a given constructor evaluates to an integer value
isInteger :: GSymbolTable -> SyntaxTree -> Bool
isInteger st (LeafVar var Deref) = getSymbolType (st Map.! var) == "intptr"
isInteger st (LeafVar var _) = getSymbolType (st Map.! var) == "int"
isInteger st (LeafFn name _) = getSymbolType (st Map.! name) == "int"
isInteger _ LeafValInt {} = True
isInteger _ NodeArmc {} = True
isInteger _ _ = False

getVarType :: GSymbolTable -> SyntaxTree -> String
getVarType st (LeafVar var Deref) = take (length t - 3) t -- Remove "ptr" from the end
  where
    t = getSymbolType (st Map.! var)
getVarType st (LeafVar var _) = getSymbolType (st Map.! var)
getVarType _ _ = error "Not a variable: "

getFnType :: GSymbolTable -> SyntaxTree -> String
getFnType st (LeafFn name _) = getSymbolType (st Map.! name)
getFnType _ _ = error "Not a function"

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
  _ -> Node (show t) []

prettyPrint :: SyntaxTree -> String
prettyPrint t = drawTree $ toDataTree t