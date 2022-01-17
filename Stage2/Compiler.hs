module Main where

import CodeGen (codeGen)
import Data.List (foldl')
import EvalTree (evalAst)
import Grammar (parseCalc)
import SyntaxTree (prettyPrint)
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode, WriteMode), hClose, hGetContents, hPutStr, openFile)
import Tokens (scanTokens)

createOutputFile :: String -> IO ()
createOutputFile code = do
  outputFile <- openFile "out.xsm" WriteMode
  hPutStr outputFile $ foldl' (\acc c -> acc ++ show c ++ "\n") "" [0, 2056, 0, 0, 0, 0, 0, 0]
  hPutStr outputFile "MOV SP, 4126\n"
  hPutStr outputFile code
  hPutStr outputFile "INT 10\n"
  hClose outputFile

main :: IO ()
main = do
  args <- getArgs
  inputFile <- openFile (head args) ReadMode
  fileContents <- hGetContents inputFile
  let ast = parseCalc (scanTokens fileContents)
  -- putStr $ prettyPrint ast
  let code = codeGen ast
  createOutputFile code
  -- putStrLn "Evaluating tree: -"
  -- evalAst ast
  hClose inputFile
