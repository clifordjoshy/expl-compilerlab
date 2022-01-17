module Main where

import CodeGen (codeGen)
import EvalTree (evalAst)
import Grammar (parseCalc)
import SyntaxTree (prettyPrint)
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode, WriteMode), hClose, hGetContents, hPutStr, openFile)
import Tokens (scanTokens)

createOutputFile :: String -> IO ()
createOutputFile code = do
  outputFile <- openFile "out.xsm" WriteMode
  hPutStr outputFile code
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
  putStrLn "Evaluating tree: -"
  evalAst ast
  hClose inputFile
