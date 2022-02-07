module XsmGen where

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
      '%' -> "MOD"
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
accessMem :: String -> String
accessMem v = "[" ++ v ++ "]"

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

-- Generates a call instruction
genCallXsm :: String -> String
genCallXsm lbl = "CALL " ++ lbl ++ "\n"

-- Generates a RET instruction
genRetXsm :: String
genRetXsm = "RET\n"

genIncSpXsm :: String
genIncSpXsm = "INR SP\n"

-- | Generates xsm for a fn call
-- | usedRegs -> FunctionLabel(accessed) -> [code for pushing each arg] -> returnReg -> Code
genFnCallXsm :: [String] -> String -> [String] -> String -> String
genFnCallXsm usedRegs fnLabel argCodes returnReg =
  pushRegCode
    ++ pushArgCode
    ++ genIncSpXsm
    ++ genCallXsm fnLabel
    ++ genStackXsm POP returnReg
    ++ popArgCode
    ++ popRegCode
  where
    pushRegCode = concatMap (genStackXsm PUSH) usedRegs
    pushArgCode = concat argCodes
    popArgCode = genArmcXsm '-' "SP" (show $length argCodes)
    popRegCode = concatMap (genStackXsm POP) (reverse usedRegs)

data LibCallArg = ValInt Int | ValString String | Reg String | None

-- | Generates library call XSM
-- | UsedRegs -> FunctionCode -> (3 arguments) -> retRegister -> Code
genLibXsm :: [String] -> String -> (LibCallArg, LibCallArg, LibCallArg) -> String -> String
genLibXsm usedRegs fnCode (a1, a2, a3) retReg = genFnCallXsm usedRegs "0" argCodes retReg
  where
    evalArg :: LibCallArg -> String
    evalArg argType = case argType of
      (ValInt val) -> genMovXsm retReg (show val) ++ genStackXsm PUSH retReg
      (ValString val) -> genMovXsm retReg (show val) ++ genStackXsm PUSH retReg
      (Reg r) -> genStackXsm PUSH r
      None -> genIncSpXsm
    fnCodeCode = genMovXsm retReg (show fnCode) ++ genStackXsm PUSH retReg
    argCodes = [fnCodeCode, evalArg a1, evalArg a2, evalArg a3]
