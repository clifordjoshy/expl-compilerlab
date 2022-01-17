module Main where

import Data.List (foldl', insertBy)
import Grammar (parseCalc)
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode, WriteMode), hClose, hGetContents, hPutStr, openFile)
import Tokens (scanTokens)
import Tree (Tree (..))

-- | freeRegisters -> (register, remainingRegisters)
-- | Get a free register
getReg :: [String] -> (String, [String])
getReg [] = error "Out of registers"
getReg (r : remainingRegs) = (r, remainingRegs)

-- | regToFree -> freeRegisters -> updatedFreeRegisters
-- | Frees a register
freeReg :: String -> [String] -> [String]
-- custom search function to insert in sorted order
freeReg = insertBy (\a b -> if length a < length b || a < b then LT else GT)

-- | operator -> leftRegister -> rightRegister -> xsmInstruction
-- | Generates XSM instruction for a specified operation
opToXsm :: Char -> String -> String -> String
opToXsm op lr rr = cmd ++ " " ++ lr ++ ", " ++ rr ++ "\n"
  where
    cmd = case op of
      '+' -> "ADD"
      '-' -> "SUB"
      '*' -> "MUL"
      '/' -> "DIV"
      _ -> error "Operator not recognized"

-- | syntaxTree -> freeRegisters -> (code, usedregister, remainingRegisters)
-- | Generates code for the given syntax tree
genCode :: Tree -> [String] -> (String, String, [String])
genCode (Leaf num) regsFree = ("MOV " ++ r ++ ", " ++ show num ++ "\n", r, remainingRegs)
  where
    (r, remainingRegs) = getReg regsFree
genCode (Node op l r) regsFree = (lCode ++ rCode ++ opToXsm op lReg rReg, lReg, freeReg rReg regRemaining2)
  where
    (lCode, lReg, regRemaining) = genCode l regsFree
    (rCode, rReg, regRemaining2) = genCode r regRemaining

createOutputFile :: String -> IO ()
createOutputFile code = do
  outputFile <- openFile "out.xsm" WriteMode
  hPutStr outputFile $ foldl' (\acc c -> acc ++ show c ++ "\n") "" [0, 2056, 0, 0, 0, 0, 0, 0]
  hPutStr outputFile code
  hPutStr outputFile "MOV R1, 5\nPUSH R1\nMOV R1, -2\nPUSH R1\nPUSH R0\nPUSH R0\nPUSH R0\nINT 7\nPOP R1\nPOP R1\nPOP R1\nPOP R1\nPOP R1\nBRKP\nINT 10\n"
  hClose outputFile

main :: IO ()
main = do
  args <- getArgs
  inputFile <- openFile (head args) ReadMode
  fileContents <- hGetContents inputFile
  let ast = parseCalc (scanTokens fileContents)
  let (code, _, _) = genCode ast ["R" ++ show i | i <- [0 .. 19]]
  putStr code
  createOutputFile code
  hClose inputFile
