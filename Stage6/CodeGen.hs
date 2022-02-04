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
  Just (lType, lAddr) -> genMovXsm r "BP" ++ genArmcXsm '+' r (show lAddr)
  Nothing -> let sym = gst Map.! name in genMovXsm r (show $ getSymbolAddress sym)

-- | Resolves a dot field and returns code. Stores final address in passed register
-- | dotlist -> reg(with sym address) -> tempReg -> Code
dotResolve :: [Int] -> String -> String -> String
dotResolve [] _ _ = ""
dotResolve (c : ds) r tempReg = code ++ dotResolve ds r tempReg
  where
    code = genMovXsm tempReg (accessMem r) ++ genArmcXsm '+' tempReg (show c) ++ genMovXsm r tempReg

-- | varName -> resolver -> freeRegs -> GSymbolTable -> LSymbolTable -> (code, reg, remainingRegs)
-- | Returns code to resolve a variable address and stores the address in a register
genAddrResolveCode :: String -> VarResolve -> [String] -> GSymbolTable -> LSymbolTable -> (String, String, [String])
genAddrResolveCode name Simple regs gst lst = (symCode, symReg, regs2)
  where
    (symReg, regs2) = getReg regs
    symCode = symbolResolve name symReg gst lst
genAddrResolveCode name Deref regs gst lst = (symCode ++ genMovXsm valReg (accessMem symReg), valReg, regs2)
  where
    (valReg, regs2) = getReg regs
    (symReg, regs3) = getReg regs2
    symCode = symbolResolve name symReg gst lst
genAddrResolveCode name (Dot dotList) regs gst lst = (symCode ++ dotCode, valReg, regs2)
  where
    (valReg, regs2) = getReg regs
    (tempReg, _) = getReg regs2
    symCode = symbolResolve name valReg gst lst
    dotCode = dotResolve dotList valReg tempReg
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

-- | varName -> resolver -> freeRegs -> GSymbolTable -> LSymbolTable -> (code, reg, remainingRegs)
-- | Returns code to resolve a variable value and stores the value in a register
genValResolveCode :: String -> VarResolve -> [String] -> GSymbolTable -> LSymbolTable -> (String, String, [String])
genValResolveCode name resolv regs gst lst = (addrCode ++ genMovXsm valReg (accessMem addrReg), valReg, rs)
  where
    (valReg, rs) = getReg regs
    (addrCode, addrReg, _) = genAddrResolveCode name resolv rs gst lst

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
-- Leaf Nodes
genCode a@Args {node = (LeafValInt num)} = (genMovXsm r $show num, r, remainingRegs, labels)
  where
    (r, remainingRegs) = getReg regsFree
    Args {regsFree = regsFree, labels = labels} = a
genCode a@Args {node = (LeafValStr val)} = (genMovXsm r val, r, remainingRegs, labels)
  where
    (r, remainingRegs) = getReg regsFree
    Args {regsFree = regsFree, labels = labels} = a
genCode a@Args {node = LeafNull} = (genMovXsm r "0", r, remainingRegs, labels)
  where
    (r, remainingRegs) = getReg regsFree
    Args {regsFree = regsFree, labels = labels} = a
genCode a@Args {node = (LeafVar var resolver)} = (code, r, rs, labels)
  where
    Args {regsFree = regsFree, labels = labels, gSymTable = gst, lSymTable = lst} = a
    (code, r, rs) = genValResolveCode var resolver regsFree gst lst
genCode a@Args {node = (LeafFn name params)} = (genFnCallXsm usedRegs fnLabel argCodes returnReg, returnReg, rs, labels)
  where
    Args {regsFree = regsFree, labels = labels, gSymTable = gst} = a
    usedRegs = getRegsUsed regsFree
    getArgCode p = let (pCode, pReg, _, _) = genCode a {node = p} in pCode ++ genStackXsm PUSH pReg
    argCodes = map getArgCode params
    fnLabel = let (Func _ _ fl) = gst Map.! name in accessLabel fl
    (returnReg, rs) = getReg regsFree

-- Operator Nodes
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

-- Conditional Execution Nodes
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
genCode a@Args {node = NodeBreak, regsFree = fr, labels = lb, blockLabels = bl} = (code, "", fr, lb)
  where
    code = case bl of
      Nothing -> ""
      Just (_, el) -> genJmpXsm el
genCode a@Args {node = NodeCont, regsFree = fr, labels = lb, blockLabels = bl} = (code, "", fr, lb)
  where
    code = case bl of
      Nothing -> ""
      Just (sl, _) -> genJmpXsm sl

-- Statement Nodes
genCode a@Args {node = (NodeAssign (LeafVar var varResolver) r)} = (code, "", regsFree, labels)
  where
    (rCode, rReg, regs2, _) = genCode a {node = r}
    (lCode, lReg, _) = genAddrResolveCode var varResolver regs2 gst lst
    code = rCode ++ lCode ++ genMovXsm (accessMem lReg) rReg
    Args {regsFree = regsFree, labels = labels, gSymTable = gst, lSymTable = lst} = a
genCode a@Args {node = (NodeConn l r)} = (lCode ++ rCode, rReg, regsFree, remainingLabels)
  where
    (lCode, _, _, labels2) = genCode a {node = l}
    (rCode, rReg, _, remainingLabels) = genCode a {node = r, labels = labels2}
    Args {regsFree = regsFree} = a
genCode a@Args {node = (NodeReturn e)} = (eCode, eReg, regsFree, labels)
  where
    (eCode, eReg, rs, _) = genCode a {node = e}
    Args {regsFree = regsFree, labels = labels} = a

-- System Call Statement Nodes
genCode a@Args {node = (NodeWrite arg)} = (argCode ++ genLibXsm usedRegs "Write" libArgs retReg, retReg, regsRem, labels)
  where
    Args {regsFree = regsFree, labels = labels} = a
    usedRegs = getRegsUsed regsFree
    (argCode, argReg, _, _) = genCode a {node = arg, regsFree = regsRem}
    libArgs = (ValInt $ -2, Reg argReg, None)
    (retReg, regsRem) = getReg regsFree
genCode a@Args {node = (NodeRead arg)} = (argCode ++ genLibXsm usedRegs "Read" libArgs retReg, retReg, regsRem, labels)
  where
    Args {regsFree = regsFree, labels = labels, gSymTable = gst, lSymTable = lst} = a
    usedRegs = getRegsUsed regsFree
    (LeafVar var varResolver) = arg
    (argCode, argReg, regRem) = genAddrResolveCode var varResolver regsRem gst lst
    libArgs = (ValInt $ -1, Reg argReg, None)
    (retReg, regsRem) = getReg regsFree
genCode a@Args {node = NodeInitialize} = (genLibXsm usedRegs "Heapset" libArgs retReg, retReg, regsRem, labels)
  where
    Args {regsFree = regsFree, labels = labels, lSymTable = lst} = a
    usedRegs = getRegsUsed regsFree
    libArgs = (None, None, None)
    (retReg, regsRem) = getReg regsFree
genCode a@Args {node = (NodeAlloc var@(LeafVar vName vRes) size)} = (code, "", regsFree, labels)
  where
    Args {regsFree = regsFree, labels = labels, gSymTable = gst, lSymTable = lst} = a
    usedRegs = getRegsUsed regsFree
    libArgs = (ValInt size, None, None)
    (retReg, regsRem) = getReg regsFree
    (lCode, lReg, _) = genAddrResolveCode vName vRes regsRem gst lst
    code = genLibXsm usedRegs "Alloc" libArgs retReg ++ lCode ++ genMovXsm (accessMem lReg) retReg
genCode a@Args {node = (NodeFree var)} = (varCode ++ genLibXsm usedRegs "Free" libArgs retReg, retReg, regsRem, labels)
  where
    Args {regsFree = regsFree, labels = labels} = a
    usedRegs = getRegsUsed regsFree
    (varCode, varReg, _, _) = genCode a {node = var, regsFree = regsRem}
    libArgs = (Reg varReg, None, None)
    (retReg, regsRem) = getReg regsFree

-- genCode a@Args {node = (NodeRef _)} = error $ "Invalid Node : " ++ show (node a)
-- genCode a@Args {node = (NodeAlloc _)} = error $ "Invalid Node : " ++ show (node a)
-- genCode a@Args {node = (NodeAssign _ _)} = error $ "Invalid Node : " ++ show (node a)
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
      ++ genMovXsm (accessMem "BP") resultReg
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
