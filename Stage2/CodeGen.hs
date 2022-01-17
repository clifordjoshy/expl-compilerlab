module CodeGen (codeGen) where

import Data.List (insertBy)
import Data.Maybe (fromMaybe)
import SyntaxTree (SyntaxTree (..))

-- | freeRegisters -> (register, remainingRegisters)
-- | Get a free register
getReg :: [String] -> (String, [String])
getReg [] = error "Out of registers"
getReg (r : remainingRegs) = (r, remainingRegs)

-- | operator -> leftRegister -> rightRegister -> xsmInstruction
-- | Generates XSM instruction for a specified arithmetic operation
genArmcXsm :: Char -> String -> String -> String
genArmcXsm op lr rr = cmd ++ " " ++ lr ++ ", " ++ rr ++ "\n"
  where
    cmd = case op of
      '+' -> "ADD"
      '-' -> "SUB"
      '*' -> "MUL"
      '/' -> "DIV"
      _ -> error "Operator not recognized"

-- | Destination -> Source -> Instruction
-- | Returns a MOV instruction from specified values
genMovXsm :: String -> String -> String
genMovXsm dest source = "MOV " ++ dest ++ ", " ++ source ++ "\n"

-- | Surrounds a given string in [] for memory access
genMemAccXsm :: String -> String
genMemAccXsm v = "[" ++ v ++ "]"

data StackOpType = PUSH | POP deriving (Show)

-- | Generates XSM for stack operations
genStackXsm :: StackOpType -> String -> String
genStackXsm opType reg = show opType ++ " " ++ reg ++ "\n"

data LibCallArg = ValInt Int | ValString String | Reg String | None

-- | Function Code -> (3 arguments) -> tempRegister -> Code
-- | Generates library call XSM
genLibXsm :: String -> (LibCallArg, LibCallArg, LibCallArg) -> String -> String
genLibXsm fnCode (a1, a2, a3) tempReg =
  evalCodeFn ++ genStackXsm PUSH cReg
    ++ (evalCode1 ++ genStackXsm PUSH arg1)
    ++ (evalCode2 ++ genStackXsm PUSH arg2)
    ++ (evalCode3 ++ genStackXsm PUSH arg3)
    ++ genStackXsm PUSH "R0"
    ++ "CALL 0\n"
    ++ genStackXsm POP tempReg
    ++ genStackXsm POP tempReg
    ++ genStackXsm POP tempReg
    ++ genStackXsm POP tempReg
    ++ genStackXsm POP tempReg
  where
    (evalCode1, arg1) = evalArg a1 tempReg
    (evalCode2, arg2) = evalArg a2 tempReg
    (evalCode3, arg3) = evalArg a3 tempReg
    (evalCodeFn, cReg) = evalArg (ValString fnCode) tempReg
    evalArg argType tempReg = case argType of
      (ValInt val) -> (genMovXsm tempReg $show val, tempReg)
      (ValString val) -> (genMovXsm tempReg $show val, tempReg)
      (Reg r) -> ("", r)
      None -> ("", "R0")

-- | syntaxTree -> freeRegisters -> (code, usedregister, remainingRegisters)
-- | Generates code for the given syntax tree
genCode :: SyntaxTree -> [String] -> (String, String, [String])
genCode (LeafVal num) regsFree = (genMovXsm r $show num, r, remainingRegs)
  where
    (r, remainingRegs) = getReg regsFree
genCode (LeafVar var addr) regsFree = (genMovXsm r $ genMemAccXsm $ show addr, r, remainingRegs)
  where
    (r, remainingRegs) = getReg regsFree
genCode (NodeArmc op l r) regsFree = (lCode ++ rCode ++ genArmcXsm op lReg rReg, lReg, regRemaining)
  where
    (lCode, lReg, regRemaining) = genCode l regsFree
    (rCode, rReg, _) = genCode r regRemaining
genCode (NodeEq l r) regsFree = (rCode ++ genMovXsm (genMemAccXsm (show varAddr)) rReg, "", regsFree)
  where
    (rCode, rReg, _) = genCode r regsFree
    (LeafVar _ varAddr) = l
genCode (NodeStmt stmt arg) regsFree = (argCode ++ genLibXsm stmt libArgs (head regRemaining), "", regsFree)
  where
    (argCode, argReg, regRemaining) = genCode arg regsFree
    libArgs = case stmt of
      "Read" -> (ValInt $ -1, ValInt varAddr, None)
      "Write" -> (ValInt $ -2, Reg argReg, None)
      _ -> error "Invalid Statement"
    (LeafVar _ varAddr) = arg
genCode (NodeConn l r) regsFree = (lCode ++ rCode, "", regsFree)
  where
    (lCode, _, _) = genCode l regsFree
    (rCode, _, _) = genCode r regsFree
genCode NodeEmpty regsFree = ("", "", regsFree)

codeGen :: SyntaxTree -> String
codeGen ast = code
  where
    (code, _, _) = genCode ast ["R" ++ show i | i <- [0 .. 19]]