module CodeGen where

import Data.List (foldl', insertBy)
import qualified Data.Map as Map
import Data.Maybe ()
import LabelLink (replaceLabels)
import SyntaxTree

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

type SymbolTable = Map.Map String (String, Int)

data CodeArgs = Args
  { node :: SyntaxTree,
    regsFree :: [String],
    labels :: [String],
    blockLabels :: Maybe (String, String),
    symTable :: SymbolTable
  }

-- | syntaxTree -> freeRegisters -> labels -> Maybe (startLabel, endLabel) -> (code, usedregister, remainingRegisters, remainingLabels)
-- | Generates code for the given syntax tree
genCode :: CodeArgs -> (String, String, [String], [String])
genCode a@Args {node = (LeafValInt num)} = (genMovXsm r $show num, r, remainingRegs, labels)
  where
    (r, remainingRegs) = getReg regsFree
    Args {regsFree = regsFree, labels = labels} = a
genCode a@Args {node = (LeafValStr val)} = (genMovXsm r val, r, remainingRegs, labels)
  where
    (r, remainingRegs) = getReg regsFree
    Args {regsFree = regsFree, labels = labels} = a
genCode a@Args {node = (LeafVar var)} = (genMovXsm r $ genMemAccXsm $ show addr, r, remainingRegs, labels)
  where
    (r, remainingRegs) = getReg regsFree
    Args {regsFree = regsFree, labels = labels, symTable = st} = a
    (_, addr) = st Map.! var
genCode a@Args {node = (NodeArmc op l r)} = (lCode ++ rCode ++ genArmcXsm op lReg rReg, lReg, regRemaining, labels)
  where
    (lCode, lReg, regRemaining, _) = genCode a {node = l}
    (rCode, rReg, _, _) = genCode a {node = r, regsFree = regRemaining}
    Args {labels = labels} = a
genCode a@Args {node = (NodeBool op l r)} = (lCode ++ rCode ++ genBoolXsm op lReg rReg, lReg, regRemaining, labels)
  where
    (lCode, lReg, regRemaining, _) = genCode a {node = l}
    (rCode, rReg, _, _) = genCode a {node = r, regsFree = regRemaining}
    Args {labels = labels} = a
genCode a@Args {node = (NodeAssign (LeafVar var) r)} = (rCode ++ genMovXsm (genMemAccXsm (show varAddr)) rReg, "", regsFree, labels)
  where
    (rCode, rReg, _, _) = if typeCheck r then genCode a {node = r} else error "Type mismatch in assignment"
    (varType, varAddr) = st Map.! var
    typeCheck = case varType of
      "int" -> isInteger
      "str" -> isString
      _ -> error "Invalid variable type"
    Args {regsFree = regsFree, labels = labels, symTable = st} = a
genCode a@Args {node = (NodeStmt "Write" arg)} = (argCode ++ genLibXsm "Write" libArgs (head regRemaining), "", regsFree, labels)
  where
    (argCode, argReg, regRemaining, _) = genCode a {node = arg}
    libArgs = (ValInt $ -2, Reg argReg, None)
    Args {regsFree = regsFree, labels = labels} = a
genCode a@Args {node = (NodeStmt "Read" arg)} = (genLibXsm "Read" libArgs (head regsFree), "", regsFree, labels)
  where
    libArgs = (ValInt $ -1, ValInt varAddr, None)
    (LeafVar var) = arg
    (varType, varAddr) = st Map.! var
    Args {regsFree = regsFree, labels = labels, symTable = st} = a
genCode a@Args {node = (NodeIf cond exec)} =
  ( genLabelXsm label ++ condCode ++ genJmpZXsm condReg endLabel ++ blockCode ++ genLabelXsm endLabel,
    "",
    regsFree,
    remainingLabels
  )
  where
    (label, labels2) = getLabel labels
    (endLabel, labels3) = getLabel labels2
    (condCode, condReg, _, _) = genCode a {node = cond}
    (blockCode, _, _, remainingLabels) = genCode a {node = exec, labels = labels3}
    Args {regsFree = regsFree, labels = labels} = a
genCode a@Args {node = (NodeIfElse cond bl1 bl2)} =
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
    (condCode, condReg, _, _) = genCode a {node = cond}
    (bl1Code, _, _, labels5) = genCode a {node = bl1, labels = labels4}
    (bl2Code, _, _, remainingLabels) = genCode a {node = bl2, labels = labels5}
    Args {regsFree = regsFree, labels = labels} = a
genCode a@Args {node = (NodeWhile cond exec)} =
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
    (condCode, condReg, _, _) = genCode a {node = cond}
    (blockCode, _, _, remainingLabels) = genCode a {node = exec, labels = labels3, blockLabels = Just (label, endLabel)}
    Args {regsFree = regsFree, labels = labels} = a
genCode Args {node = NodeBreak, regsFree = fr, labels = lb, blockLabels = bl} = (code, "", fr, lb)
  where
    code = case bl of
      Nothing -> ""
      (Just (_, el)) -> genJmpXsm el
genCode Args {node = NodeCont, regsFree = fr, labels = lb, blockLabels = bl} = (code, "", fr, lb)
  where
    code = case bl of
      Nothing -> ""
      (Just (sl, _)) -> genJmpXsm sl
genCode a@Args {node = (NodeConn l r)} = (lCode ++ rCode, "", regsFree, remainingLabels)
  where
    (lCode, _, _, labels2) = genCode a {node = l}
    (rCode, _, _, remainingLabels) = genCode a {node = r, labels = labels2}
    Args {regsFree = regsFree} = a
genCode Args {node = NodeEmpty, regsFree = rf, labels = lb} = ("", "", rf, lb)
genCode a = error $ "Invalid Node : " ++ show (node a)

-- | Array of decls ("type": [vars]) -> (SymbolTable, freeAddr)
genSymbolTable :: [(String, [String])] -> (SymbolTable, Int)
genSymbolTable [] = (Map.empty, 4096)
genSymbolTable ((t, names) : ds) = (Map.unionWith nameError map1 map2, addr + length names)
  where
    (map1, addr) = genSymbolTable ds
    map2 = Map.fromListWith nameError [(n, (t, a)) | (n, a) <- zip names [addr ..]]
    nameError = error "Non-unique variable declaration"

-- | AST -> Variables -> Code
codeGen :: SyntaxTree -> [(String, [String])] -> String
codeGen ast vars = foldl' (\acc c -> acc ++ show c ++ "\n") "" [0, 2056, 0, 0, 0, 0, 0, 0] ++ code
  where
    (symTable, sp) = genSymbolTable vars
    (codeLabel, _, _, _) =
      genCode
        Args
          { node = ast,
            regsFree = ["R" ++ show i | i <- [0 .. 19]],
            labels = ["L" ++ show i | i <- [0, 1 ..]],
            blockLabels = Nothing,
            symTable = symTable
          }
    code = replaceLabels (genMovXsm "SP" (show sp) ++ codeLabel ++ "INT 10\n")