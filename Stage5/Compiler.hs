module Main where

-- import CodeGen (codeGen)
import Grammar
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
  -- let (gSymTable, addr, _) = genSymbolTable gDeclBlock
  print gTable
  -- let code = codeGen gDeclBlock fDefBlock mainBlock
  -- createOutputFile code
  hClose inputFile
