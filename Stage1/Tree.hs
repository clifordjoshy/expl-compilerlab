module Tree where

data Tree = Leaf Int | Node Char Tree Tree deriving (Show)

postfix :: Tree -> String
postfix (Leaf num) = show num
postfix (Node val l r) = postfix l ++ " " ++ postfix r ++ " " ++ [val]

prefix :: Tree -> String
prefix (Leaf num) = show num
prefix (Node val l r) = [val] ++ " " ++ prefix l ++ " " ++ prefix r