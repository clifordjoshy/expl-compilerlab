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

-- Generates a call instruction
genCallXsm :: String -> String
genCallXsm lbl = "CALL " ++ lbl ++ "\n"

-- Generates a RET instruction
genRetXsm :: String
genRetXsm = "RET\n"

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
    ++ genCallXsm "0"
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
