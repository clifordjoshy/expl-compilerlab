module SyntaxTree (SyntaxTree (..), prettyPrint) where

import Data.Tree (Tree (Node), drawTree)

data SyntaxTree
  = LeafVar String Int
  | LeafVal Int
  | NodeStmt String SyntaxTree
  | NodeArmc Char SyntaxTree SyntaxTree
  | NodeBool String SyntaxTree SyntaxTree
  | NodeAssign SyntaxTree SyntaxTree
  | NodeConn SyntaxTree SyntaxTree
  | NodeIf SyntaxTree SyntaxTree
  | NodeIfElse SyntaxTree SyntaxTree SyntaxTree
  | NodeWhile SyntaxTree SyntaxTree
  | NodeBreak
  | NodeCont
  | NodeEmpty
  deriving (Show)

toDataTree :: SyntaxTree -> Tree String
toDataTree t = case t of
  LeafVar _ _ -> Node (show t) []
  LeafVal _ -> Node (show t) []
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

prettyPrint :: SyntaxTree -> String
prettyPrint t = drawTree $ toDataTree t