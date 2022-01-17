module EvalTree (evalAst) where

import Control.Monad (void, when)
import Data.Char (ord)
import Distribution.Compat.Lens (_1)
import SyntaxTree (SyntaxTree (..))

-- DOES NOT HANDLE BREAK, CONTINUE ONWARDS

evaluate :: SyntaxTree -> [Int] -> IO (Int, [Int])
evaluate (LeafVal num) vars = do return (num, vars)
evaluate (LeafVar var _) vars = do return (vars !! (ord (head var) - 97), vars)
evaluate (NodeArmc op lc rc) vars = do
  (l, _) <- evaluate lc vars
  (r, _) <- evaluate rc vars
  let res = case op of
        '+' -> l + r
        '-' -> l - r
        '*' -> l * r
        '/' -> l `div` r
        _ -> error "Invalid arithmetic operator"
  return (res, vars)
evaluate (NodeBool op lc rc) vars = do
  (l, _) <- evaluate lc vars
  (r, _) <- evaluate rc vars

  let res = case op of
        "<" -> l < r
        ">" -> l > r
        ">=" -> l >= r
        "<=" -> l <= r
        "==" -> l == r
        "!=" -> l /= r
        _ -> error "Invalid boolean operator"
  return (fromEnum res, vars)
evaluate (NodeAssign lc rc) vars = do
  let LeafVar varName _ = lc
  (val, _) <- evaluate rc vars
  let (a, _ : b) = splitAt (ord (head varName) - 97) vars
  return (val, a ++ val : b)
evaluate (NodeStmt "Read" arg) vars = do
  input <- getLine
  let LeafVar varName _ = arg
  let (a, _ : b) = splitAt (ord (head varName) - 97) vars
  let val = read input
  return (val, a ++ val : b)
evaluate (NodeStmt "Write" arg) vars = do
  (val, _) <- evaluate arg vars
  print val
  return (0, vars)
evaluate (NodeStmt stmt arg) vars = do error $ "Invalid statement: " ++ stmt
evaluate (NodeIf cond exe) vars = do
  (res, _) <- evaluate cond vars
  (_, vars2) <- if res == 1 then evaluate exe vars else return (0, vars)
  return (0, vars2)
evaluate (NodeIfElse cond bl1 bl2) vars = do
  (res, _) <- evaluate cond vars
  (_, vars2) <- if res == 1 then evaluate bl1 vars else evaluate bl2 vars
  return (0, vars2)
evaluate (NodeWhile cond exe) vars = do
  (res, _) <- evaluate cond vars
  if res == 1
    then do
      (_, vars2) <- evaluate exe vars
      (_, vars3) <- evaluate (NodeWhile cond exe) vars2
      return (0, vars3)
    else return (0, vars)
evaluate (NodeConn lc rc) vars = do
  (l, vars1) <- evaluate lc vars
  (r, vars2) <- evaluate rc vars1
  return (0, vars2)
evaluate NodeEmpty vars = do return (0, vars)

evalAst :: SyntaxTree -> IO ()
evalAst ast = do
  evaluate ast (replicate 26 0)
  return ()