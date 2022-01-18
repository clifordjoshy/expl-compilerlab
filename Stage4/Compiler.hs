module Main where

import CodeGen (codeGen)
import Grammar (parseCalc)
import SymbolTable (genSymbolTable)
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
  let (vars, ast) = parseCalc (scanTokens fileContents)
  -- putStr $ prettyPrint ast
  -- print $ genSymbolTable vars
  let code = codeGen ast vars
  createOutputFile code
  hClose inputFile
