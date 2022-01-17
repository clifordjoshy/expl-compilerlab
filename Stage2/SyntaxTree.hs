module SyntaxTree (SyntaxTree (..), prettyPrint) where

import Data.Tree (Tree (Node), drawTree)

data SyntaxTree
  = LeafVar String Int
  | LeafVal Int
  | NodeStmt String SyntaxTree
  | NodeArmc Char SyntaxTree SyntaxTree
  | NodeEq SyntaxTree SyntaxTree
  | NodeConn SyntaxTree SyntaxTree
  | NodeEmpty
  deriving (Show)

toDataTree :: SyntaxTree -> Tree String
toDataTree t = case t of
  LeafVar _ _ -> Node (show t) []
  LeafVal _ -> Node (show t) []
  NodeStmt c s -> Node c [toDataTree s]
  NodeArmc c l r -> Node ("Operator " ++ [c]) [toDataTree l, toDataTree r]
  NodeConn l r -> Node "NodeConn" [toDataTree l, toDataTree r]
  NodeEq l r -> Node "Assign" [toDataTree l, toDataTree r]
  NodeEmpty -> Node "Empty" []

prettyPrint :: SyntaxTree -> String
prettyPrint t = drawTree $ toDataTree t
