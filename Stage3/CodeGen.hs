module CodeGen where

import Data.List (foldl', insertBy)
import Data.Maybe ()
import LabelLink (replaceLabels)
import SyntaxTree (SyntaxTree (..))

-- | freeRegisters -> (register, remainingRegisters)
-- | Get a free register
getReg :: [String] -> (String, [String])
getReg [] = error "Out of registers"
getReg (r : remainingRegs) = (r, remainingRegs)

-- | labels -> (label, remainingLabels)
-- | Get a free label
getLabel :: [String] -> (String, [String])
getLabel [] = error "Label Not Available"
getLabel (l : remainingLabels) = (l, remainingLabels)

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
      _ -> error "Invalid Arithmetic Operator"

-- | operator -> leftRegister -> rightRegister -> xsmInstruction
-- | Generates XSM instruction for a specified boolean operation
genBoolXsm :: String -> String -> String -> String
genBoolXsm op lr rr = cmd ++ " " ++ lr ++ ", " ++ rr ++ "\n"
  where
    cmd = case op of
      "<" -> "LT"
      ">" -> "GT"
      "<=" -> "LE"
      ">=" -> "GE"
      "==" -> "EQ"
      "!=" -> "NE"
      _ -> error "Invalid Boolean Operator"

-- | Destination -> Source -> Instruction
-- | Returns a MOV instruction from specified values
genMovXsm :: String -> String -> String
genMovXsm dest source = "MOV " ++ dest ++ ", " ++ source ++ "\n"

-- | Surrounds a given string in [] for memory access
genMemAccXsm :: String -> String
genMemAccXsm v = "[" ++ v ++ "]"

-- Appends : to the end defining a label
genLabelXsm :: String -> String
genLabelXsm l = l ++ ":"

-- Surrounds label with < >
accessLabel :: String -> String
accessLabel l = "<" ++ l ++ ">"

-- | Register -> Label -> Instruction
-- | Returns a JZ instruction to the specified label with value in reg
genJmpZXsm :: String -> String -> String
genJmpZXsm r l = "JZ " ++ r ++ ", " ++ accessLabel l ++ "\n"

-- | Label -> Instruction
-- | Returns a JMP instruction to the specified label
genJmpXsm :: String -> String
genJmpXsm l = "JMP " ++ accessLabel l ++ "\n"

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

-- | syntaxTree -> freeRegisters -> labels -> Maybe (startLabel, endLabel) -> (code, usedregister, remainingRegisters, remainingLabels)
-- | Generates code for the given syntax tree
genCode :: SyntaxTree -> [String] -> [String] -> Maybe (String, String) -> (String, String, [String], [String])
genCode (LeafVal num) regsFree labels _ = (genMovXsm r $show num, r, remainingRegs, labels)
  where
    (r, remainingRegs) = getReg regsFree
genCode (LeafVar var addr) regsFree labels _ = (genMovXsm r $ genMemAccXsm $ show addr, r, remainingRegs, labels)
  where
    (r, remainingRegs) = getReg regsFree
genCode (NodeArmc op l r) regsFree labels _ = (lCode ++ rCode ++ genArmcXsm op lReg rReg, lReg, regRemaining, labels)
  where
    (lCode, lReg, regRemaining, _) = genCode l regsFree [] Nothing
    (rCode, rReg, _, _) = genCode r regRemaining [] Nothing
genCode (NodeBool op l r) regsFree labels _ = (lCode ++ rCode ++ genBoolXsm op lReg rReg, lReg, regRemaining, labels)
  where
    (lCode, lReg, regRemaining, _) = genCode l regsFree [] Nothing
    (rCode, rReg, _, _) = genCode r regRemaining [] Nothing
genCode (NodeAssign l r) regsFree labels _ = (rCode ++ genMovXsm (genMemAccXsm (show varAddr)) rReg, "", regsFree, labels)
  where
    (rCode, rReg, _, _) = genCode r regsFree [] Nothing
    (LeafVar _ varAddr) = l
genCode (NodeStmt stmt arg) regsFree labels _ = (argCode ++ genLibXsm stmt libArgs (head regRemaining), "", regsFree, labels)
  where
    (argCode, argReg, regRemaining, _) = genCode arg regsFree [] Nothing
    libArgs = case stmt of
      "Read" -> (ValInt $ -1, ValInt varAddr, None)
      "Write" -> (ValInt $ -2, Reg argReg, None)
      _ -> error "Invalid Statement"
    (LeafVar _ varAddr) = arg
genCode (NodeIf cond exec) regsFree labels loopL =
  ( genLabelXsm label ++ condCode ++ genJmpZXsm condReg endLabel ++ blockCode
      ++ genLabelXsm endLabel,
    "",
    regsFree,
    remainingLabels
  )
  where
    (label, labels2) = getLabel labels
    (endLabel, labels3) = getLabel labels2
    (condCode, condReg, _, _) = genCode cond regsFree [] Nothing
    (blockCode, _, _, remainingLabels) = genCode exec regsFree labels3 loopL
genCode (NodeIfElse cond bl1 bl2) regsFree labels loopL =
  ( genLabelXsm label ++ condCode ++ genJmpZXsm condReg elseLabel ++ bl1Code ++ genJmpXsm endLabel
      ++ genLabelXsm elseLabel
      ++ bl2Code
      ++ genLabelXsm endLabel,
    "",
    regsFree,
    remainingLabels
  )
  where
    (label, labels2) = getLabel labels
    (elseLabel, labels3) = getLabel labels2
    (endLabel, labels4) = getLabel labels3
    (condCode, condReg, _, _) = genCode cond regsFree [] Nothing
    (bl1Code, _, _, labels5) = genCode bl1 regsFree labels4 loopL
    (bl2Code, _, _, remainingLabels) = genCode bl2 regsFree labels5 loopL
genCode (NodeWhile cond exec) regsFree labels _ =
  ( genLabelXsm label ++ condCode ++ genJmpZXsm condReg endLabel ++ blockCode
      ++ genJmpXsm label
      ++ genLabelXsm endLabel,
    "",
    regsFree,
    remainingLabels
  )
  where
    (label, labels2) = getLabel labels
    (endLabel, labels3) = getLabel labels2
    (condCode, condReg, _, _) = genCode cond regsFree [] Nothing
    (blockCode, _, _, remainingLabels) = genCode exec regsFree labels3 (Just (label, endLabel))
genCode NodeBreak fr lb Nothing = ("", "", fr, lb)
genCode NodeBreak fr lb (Just (_, el)) = (genJmpXsm el, "", fr, lb)
genCode NodeCont fr lb Nothing = ("", "", fr, lb)
genCode NodeCont fr lb (Just (sl, _)) = (genJmpXsm sl, "", fr, lb)
genCode (NodeConn l r) regsFree labels loopL = (lCode ++ rCode, "", regsFree, remainingLabels)
  where
    (lCode, _, _, labels2) = genCode l regsFree labels loopL
    (rCode, _, _, remainingLabels) = genCode r regsFree labels2 loopL
genCode NodeEmpty regsFree labels _ = ("", "", regsFree, labels)

codeGen :: SyntaxTree -> String
codeGen ast = foldl' (\acc c -> acc ++ show c ++ "\n") "" [0, 2056, 0, 0, 0, 0, 0, 0] ++ code
  where
    (codeLabel, _, _, _) = genCode ast ["R" ++ show i | i <- [0 .. 19]] ["L" ++ show i | i <- [0, 1 ..]] Nothing
    code = replaceLabels ("MOV SP, 4126\n" ++ codeLabel ++ "INT 10\n")