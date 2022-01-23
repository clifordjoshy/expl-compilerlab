module Main where

import CodeGen (codeGen)
import Parser (parseTokens)
import SymbolTable
import SyntaxTree
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
  let (gTable, sp, fDecl, main) = parseTokens (scanTokens fileContents)
  -- putStr $ prettyPrint ast
  -- print fDecl
  let code = codeGen gTable sp fDecl main
  createOutputFile code
  hClose inputFile
