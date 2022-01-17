module SyntaxTree (SyntaxTree (..), prettyPrint, isInteger, isString, isBoolean) where

import Data.Tree (Tree (Node), drawTree)

data SyntaxTree
  = LeafVar String
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
  | NodeBreak
  | NodeCont
  | NodeEmpty
  deriving (Show)

-- | Returns if a given constructor evaluates to an integer value
isInteger :: SyntaxTree -> Bool
isInteger LeafVar {} = True
isInteger LeafValInt {} = True
isInteger NodeArmc {} = True
isInteger _ = False

-- | Returns if a given constructor evaluates to a boolean value
isBoolean :: SyntaxTree -> Bool
isBoolean NodeBool {} = True
isBoolean _ = False

-- | Returns if a given constructor evaluates to a string value
isString :: SyntaxTree -> Bool
isString LeafValStr {} = True
isString _ = False

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