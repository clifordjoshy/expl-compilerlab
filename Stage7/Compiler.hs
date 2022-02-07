module Main where

import CodeGen (codeGen)
import Parser (parseTokens)
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
  let (cTable, cfDecls, gTable, sp, fDecl, main) = parseTokens (scanTokens fileContents)
  print cTable
  let (_, mainAst) = main in putStr $ prettyPrint mainAst
  -- let code = codeGen gTable sp fDecl main
  -- createOutputFile code
  hClose inputFile
