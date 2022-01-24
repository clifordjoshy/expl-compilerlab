{-# OPTIONS_GHC -Wno-missing-fields #-}

module CodeGen where

import Data.List
import qualified Data.Map as Map
import LabelLink (replaceLabels)
import SymbolTable
import SyntaxTree
import XsmGen

allRegs :: [String]
allRegs = ["R" ++ show i | i <- [0 .. 19]]

-- | freeRegisters -> (register, remainingRegisters)
-- | Get a free register
getReg :: [String] -> (String, [String])
getReg [] = error "Out of registers"
getReg (r : remainingRegs) = (r, remainingRegs)

-- | freeRegisters -> usedRegisters
-- | Returns the list of used registers
getRegsUsed :: [String] -> [String]
getRegsUsed freeRegs = allRegs \\ freeRegs

-- | resolves a symbol name and returns the address in a register
-- | symName -> reg -> globalSymbolT -> localSymbolT -> (code, sym)
symbolResolve :: String -> String -> GSymbolTable -> LSymbolTable -> String
symbolResolve name r gst lst = case Map.lookup name lst of
  Just (_, lAddr) -> genMovXsm r "BP" ++ genArmcXsm '+' r (show lAddr)
  Nothing -> genMovXsm r (show $ getSymbolAddress (gst Map.! name))

-- | varName -> resolver -> freeRegs -> GSymbolTable -> LSymbolTable -> (code, reg, remainingRegs)
-- | Returns code to resolve a variable value and stores the address in a register
genValResolveCode :: String -> VarResolve -> [String] -> GSymbolTable -> LSymbolTable -> (String, String, [String])
genValResolveCode name Simple regs gst lst = (symCode ++ genMovXsm valReg (genMemAccXsm symReg), valReg, regs2)
  where
    (valReg, regs2) = getReg regs
    (symReg, _) = getReg regs2
    symCode = symbolResolve name symReg gst lst
genValResolveCode name Deref regs gst lst =
  ( symCode
      ++ genMovXsm tempReg (genMemAccXsm symReg)
      ++ genMovXsm valReg (genMemAccXsm tempReg),
    valReg,
    regs2
  )
  where
    (valReg, regs2) = getReg regs
    (symReg, regs3) = getReg regs2
    (tempReg, _) = getReg regs3
    symCode = symbolResolve name symReg gst lst
genValResolveCode name (Index i) regs gst lst =
  ( symCode
      ++ iCode
      ++ genArmcXsm '+' iReg symReg
      ++ genMovXsm valReg (genMemAccXsm iReg),
    valReg,
    regs2
  )
  where
    (valReg, regs2) = getReg regs
    (symReg, regs3) = getReg regs2
    symCode = symbolResolve name symReg gst lst
    (iCode, iReg, _, _) = genCode Args {node = i, regsFree = regs3, gSymTable = gst, lSymTable = lst, labels = [], blockLabels = Nothing}
genValResolveCode name (Index2D i j) regs gst lst =
  ( symCode
      ++ iCode
      ++ jCode
      ++ genArmcXsm '*' iReg (show n)
      ++ genArmcXsm '+' iReg jReg
      ++ genArmcXsm '+' iReg symReg
      ++ genMovXsm valReg (genMemAccXsm iReg),
    valReg,
    regs2
  )
  where
    (valReg, regs2) = getReg regs
    (symReg, regs3) = getReg regs2
    (Arr2 _ m n _) = gst Map.! name
    symCode = symbolResolve name symReg gst lst
    (iCode, iReg, regs4, _) = genCode Args {node = i, regsFree = regs3, gSymTable = gst, lSymTable = lst, labels = [], blockLabels = Nothing}
    (jCode, jReg, _, _) = genCode Args {node = j, regsFree = regs4, gSymTable = gst, lSymTable = lst, labels = [], blockLabels = Nothing}

-- | varName -> resolver -> freeRegs -> GSymbolTable -> LSymbolTable -> (code, reg, remainingRegs)
-- | Returns code to resolve a variable address and stores the address in a register
genAddrResolveCode :: String -> VarResolve -> [String] -> GSymbolTable -> LSymbolTable -> (String, String, [String])
genAddrResolveCode name Simple regs gst lst = (symCode, symReg, regs2)
  where
    (symReg, regs2) = getReg regs
    symCode = symbolResolve name symReg gst lst
genAddrResolveCode name Deref regs gst lst =
  ( symCode ++ genMovXsm valReg (genMemAccXsm symReg),
    valReg,
    regs2
  )
  where
    (valReg, regs2) = getReg regs
    (symReg, regs3) = getReg regs2
    symCode = symbolResolve name symReg gst lst
genAddrResolveCode name (Index i) regs gst lst =
  ( symCode
      ++ iCode
      ++ genArmcXsm '+' symReg iReg,
    symReg,
    regs2
  )
  where
    (symReg, regs2) = getReg regs
    (iCode, iReg, _, _) = genCode Args {node = i, regsFree = regs2, gSymTable = gst, lSymTable = lst}
    symCode = symbolResolve name symReg gst lst
genAddrResolveCode name (Index2D i j) regs gst lst =
  ( symCode
      ++ iCode
      ++ jCode
      ++ genArmcXsm '*' iReg (show n)
      ++ genArmcXsm '+' iReg jReg
      ++ genArmcXsm '+' symReg iReg,
    symReg,
    regs2
  )
  where
    (symReg, regs2) = getReg regs
    (iCode, iReg, regs3, _) = genCode Args {node = i, regsFree = regs2, gSymTable = gst, lSymTable = lst, labels = [], blockLabels = Nothing}
    symCode = symbolResolve name symReg gst lst
    (Arr2 _ m n _) = gst Map.! name
    (jCode, jReg, _, _) = genCode Args {node = j, regsFree = regs3, gSymTable = gst, lSymTable = lst, labels = [], blockLabels = Nothing}

data CodeArgs = Args
  { node :: SyntaxTree,
    regsFree :: [String],
    labels :: [String],
    blockLabels :: Maybe (String, String),
    gSymTable :: GSymbolTable,
    lSymTable :: LSymbolTable
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
genCode a@Args {node = (LeafVar var resolver)} = (code, r, rs, labels)
  where
    Args {regsFree = regsFree, labels = labels, gSymTable = gst, lSymTable = lst} = a
    (code, r, rs) = genValResolveCode var resolver regsFree gst lst
genCode a@Args {node = (LeafFn name params)} =
  ( pushRegCode
      ++ pushArgCode
      ++ genStackXsm PUSH "R0"
      ++ genCallXsm (accessLabel fnLabel)
      ++ genStackXsm POP returnReg
      ++ popArgCode
      ++ popRegCode,
    returnReg,
    rs,
    labels
  )
  where
    Args {regsFree = regsFree, labels = labels, gSymTable = gst} = a
    usedRegs = getRegsUsed regsFree
    pushRegCode = concatMap (genStackXsm PUSH) usedRegs
    getArgCode p = let (pCode, pReg, _, _) = genCode a {node = p} in pCode ++ genStackXsm PUSH pReg
    pushArgCode = concatMap getArgCode params
    (Func _ _ fnLabel) = gst Map.! name
    (returnReg, rs) = getReg regsFree
    popArgCode = concat $ replicate (length params) $ genStackXsm POP (head rs)
    popRegCode = concatMap (genStackXsm POP) (reverse usedRegs)
genCode a@Args {node = (NodeRef (LeafVar var resolver))} = (argCode, r, rem, labels)
  where
    Args {regsFree = regsFree, labels = labels, gSymTable = gst, lSymTable = lst} = a
    (argCode, r, rem) = genAddrResolveCode var resolver regsFree gst lst
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
genCode a@Args {node = (NodeAssign (LeafVar var varResolver) r)} = (rCode ++ lCode ++ genMovXsm (genMemAccXsm lReg) rReg, "", regsFree, labels)
  where
    (rCode, rReg, regs2, _) = genCode a {node = r}
    (lCode, lReg, _) = genAddrResolveCode var varResolver regs2 gst lst
    Args {regsFree = regsFree, labels = labels, gSymTable = gst, lSymTable = lst} = a
genCode a@Args {node = (NodeStmt "Write" arg)} = (argCode ++ genLibXsm "Write" libArgs (head regRemaining), "", regsFree, labels)
  where
    (argCode, argReg, regRemaining, _) = genCode a {node = arg}
    libArgs = (ValInt $ -2, Reg argReg, None)
    Args {regsFree = regsFree, labels = labels} = a
genCode a@Args {node = (NodeStmt "Read" arg)} = (argCode ++ genLibXsm "Read" libArgs (head regRem), "", regsFree, labels)
  where
    libArgs = (ValInt $ -1, Reg argReg, None)
    (LeafVar var varResolver) = arg
    (argCode, argReg, regRem) = genAddrResolveCode var varResolver regsFree gst lst
    Args {regsFree = regsFree, labels = labels, gSymTable = gst, lSymTable = lst} = a
genCode a@Args {node = (NodeIf cond exec)} =
  ( condCode ++ genJmpZXsm condReg endLabel ++ blockCode ++ genLabelXsm endLabel,
    "",
    regsFree,
    remainingLabels
  )
  where
    endLabel : labels2 = labels
    (condCode, condReg, _, _) = genCode a {node = cond}
    (blockCode, _, _, remainingLabels) = genCode a {node = exec, labels = labels2}
    Args {regsFree = regsFree, labels = labels} = a
genCode a@Args {node = (NodeIfElse cond bl1 bl2)} =
  ( condCode ++ genJmpZXsm condReg elseLabel ++ bl1Code ++ genJmpXsm endLabel
      ++ genLabelXsm elseLabel
      ++ bl2Code
      ++ genLabelXsm endLabel,
    "",
    regsFree,
    remainingLabels
  )
  where
    elseLabel : labels2 = labels
    endLabel : labels3 = labels2
    (condCode, condReg, _, _) = genCode a {node = cond}
    (bl1Code, _, _, labels5) = genCode a {node = bl1, labels = labels3}
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
    label : labels2 = labels
    endLabel : labels3 = labels2
    (condCode, condReg, _, _) = genCode a {node = cond}
    (blockCode, _, _, remainingLabels) = genCode a {node = exec, labels = labels3, blockLabels = Just (label, endLabel)}
    Args {regsFree = regsFree, labels = labels} = a
genCode Args {node = NodeBreak, regsFree = fr, labels = lb, blockLabels = bl} = (code, "", fr, lb)
  where
    code = case bl of
      Nothing -> ""
      Just (_, el) -> genJmpXsm el
genCode Args {node = NodeCont, regsFree = fr, labels = lb, blockLabels = bl} = (code, "", fr, lb)
  where
    code = case bl of
      Nothing -> ""
      Just (sl, _) -> genJmpXsm sl
genCode a@Args {node = (NodeConn l r)} = (lCode ++ rCode, rReg, regsFree, remainingLabels)
  where
    (lCode, _, _, labels2) = genCode a {node = l}
    (rCode, rReg, _, remainingLabels) = genCode a {node = r, labels = labels2}
    Args {regsFree = regsFree} = a
genCode a@Args {node = (NodeReturn e)} = (eCode, eReg, regsFree, labels)
  where
    (eCode, eReg, rs, _) = genCode a {node = e}
    Args {regsFree = regsFree, labels = labels} = a
genCode a = error $ "Invalid Node : " ++ show (node a)

type FDef = (String, String, [String], LSymbolTable, SyntaxTree)

-- Generates code for a function
-- FDef -> GSymbolTable -> labels -> (remainingLabels, code)
genFnCode :: FDef -> GSymbolTable -> [String] -> ([String], String)
genFnCode (_, name, params, lSym, ast) gSym labels =
  ( remainingLabels,
    genLabelXsm label
      ++ genStackXsm PUSH "BP"
      ++ genMovXsm "BP" "SP"
      ++ genArmcXsm '+' "BP" "1"
      ++ lVarAllocCode
      ++ astCode
      ++ genArmcXsm '-' "BP" "3"
      ++ genMovXsm (genMemAccXsm "BP") resultReg
      ++ lVarRelCode
      ++ genStackXsm POP "BP"
      ++ genRetXsm
  )
  where
    (Func _ _ label) = gSym Map.! name
    lVarCount = Map.size lSym - length params
    lVarAllocCode = concat $ replicate lVarCount (genStackXsm PUSH "R0")
    (astCode, resultReg, _, remainingLabels) =
      genCode
        Args
          { node = ast,
            regsFree = allRegs,
            labels = labels,
            blockLabels = Nothing,
            gSymTable = gSym,
            lSymTable = lSym
          }
    lVarRelCode = concat $ replicate lVarCount (genStackXsm POP "R0")

-- GSymbolTable -> sp -> fDecls -> mainBlock -> code
codeGen :: GSymbolTable -> Int -> [FDef] -> (LSymbolTable, SyntaxTree) -> String
codeGen gTable sp fDecls main = header ++ code
  where
    header = unlines $ map show [0, 2056, 0, 0, 0, 0, 0, 0]
    (labelsRem, codeList) = mapAccumL (\a b -> genFnCode b gTable a) ["L" ++ show i | i <- [0, 1 ..]] fDecls
    (mainLSym, mainAst) = main
    (_, mainCode) = genFnCode ("int", "main", [], mainLSym, mainAst) gTable labelsRem
    (initCode, _, _, _) = genCode Args {node = LeafFn "main" [], regsFree = allRegs, gSymTable = gTable}
    labelledCode = genMovXsm "SP" (show sp) ++ initCode ++ "INT 10\n" ++ concat codeList ++ mainCode
    -- code = labelledCode
    code = replaceLabels labelledCode
