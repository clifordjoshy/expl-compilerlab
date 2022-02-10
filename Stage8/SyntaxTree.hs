module SyntaxTree (SyntaxTree (..), VarResolve (..), prettyPrint, isInteger, getVarType, getFnType) where

import qualified Data.Map as Map
import Data.Tree (Tree (Node), drawTree)
import DefTables
import SymbolTable

data VarResolve
  = Simple
  | Deref
  | Index SyntaxTree
  | Index2D SyntaxTree SyntaxTree
  | Dot [Int]
  deriving (Show)

data SyntaxTree
  = LeafVar String VarResolve -- VarName, Resolver
  | LeafFn String [SyntaxTree] -- FnLabel, Params
  | LeafMtd (String, Maybe Symbol) String [SyntaxTree] -- (InstanceName, Offset), FnLabel, Params
  | LeafValInt Int
  | LeafValStr String
  | LeafNull
  | NodeRead SyntaxTree
  | NodeWrite SyntaxTree
  | NodeInitialize
  | NodeAlloc SyntaxTree Int -- AllocVar, AllocSize
  | NodeNew SyntaxTree Int String -- AllocVar, Size, FnTableIndex
  | NodeFree SyntaxTree
  | NodeArmc Char SyntaxTree SyntaxTree
  | NodeBool String SyntaxTree SyntaxTree
  | NodeAssign SyntaxTree SyntaxTree
  | NodeAssignC String SyntaxTree SyntaxTree -- for class inst assignment(rClass, left, right)
  | NodeConn SyntaxTree SyntaxTree
  | NodeIf SyntaxTree SyntaxTree
  | NodeIfElse SyntaxTree SyntaxTree SyntaxTree
  | NodeWhile SyntaxTree SyntaxTree
  | NodeRef SyntaxTree
  | NodeBreak
  | NodeCont
  | NodeReturn SyntaxTree
  deriving (Show)

-- | Returns if a given constructor evaluates to an integer value in the context of passed symboltable
isInteger :: GSymbolTable -> TypeTable -> ClassTable -> SyntaxTree -> Bool
isInteger st tt ct v@(LeafVar name _) = getVarType st tt v == "int"
isInteger st _ ct f@LeafFn {} = getFnType st ct f == "int"
isInteger st _ ct f@LeafMtd {} = getFnType st ct f == "int"
isInteger _ _ _ LeafValInt {} = True
isInteger _ _ _ NodeArmc {} = True
isInteger _ _ _ _ = False

-- Used in ParserState. Takes merged sym table. (check lvalue)
-- gSymT/cSymT -> TypeTable  -> varNode -> Type
getVarType :: GSymbolTable -> TypeTable -> SyntaxTree -> String
getVarType st _ (LeafVar var Deref) = init $ getSymbolType (st Map.! var) -- Remove "*" from the end
getVarType cst tt (LeafVar "self" (Dot (attrAddr : dotList))) = resolveDotType tt (getSymbolType attrSym) dotList
  where
    filterFn s = isUnit s && (getSymbolAddress s == attrAddr)
    attrSym = head (Map.elems (Map.filter filterFn cst))
getVarType st tt (LeafVar var (Dot dotList)) = resolveDotType tt (getSymbolType (st Map.! var)) dotList
getVarType st _ (LeafVar var _) = getSymbolType (st Map.! var)
getVarType _ _ _ = error "Not a variable"

-- Takes merged sym table
getFnType :: GSymbolTable -> ClassTable -> SyntaxTree -> String
getFnType st _ (LeafFn label _) = getSymbolType (st Map.! label)
getFnType st ct (LeafMtd (ins, Nothing) label _) = getSymbolType fSym
  where
    cName = getSymbolType (st Map.! ins)
    Class {st = cst} = ct Map.! cName
    filterFn f = isFunc f && (getFuncLabel f == label)
    fSym = head (Map.elems (Map.filter filterFn cst))
getFnType _ ct (LeafMtd (sym, Just s) label _) = getSymbolType fSym
  where
    Class {st = cst} = ct Map.! getSymbolType s
    filterFn f = isFunc f && (getFuncLabel f == label)
    fSym = head (Map.elems (Map.filter filterFn cst))
getFnType st _ (NodeRead _) = "int"
getFnType st _ (NodeWrite _) = "int"
getFnType st _ NodeInitialize = "int"
getFnType st _ (NodeFree _) = "int"
getFnType st _ (NodeAlloc _ _) = "int"
getFnType _ _ _ = error "Not a function"

toDataTree :: SyntaxTree -> Tree String
toDataTree t = case t of
  NodeArmc c l r -> Node ("Arithmetic " ++ [c]) [toDataTree l, toDataTree r]
  NodeBool c l r -> Node ("Boolean " ++ c) [toDataTree l, toDataTree r]
  NodeConn l r -> Node "NodeConn" [toDataTree l, toDataTree r]
  NodeAssign l r -> Node "Assign" [toDataTree l, toDataTree r]
  NodeAssignC c l r -> Node ("AssignInstance " ++ c) [toDataTree l, toDataTree r]
  NodeIf cond bl -> Node "If" [toDataTree cond, toDataTree bl]
  NodeIfElse cond bl1 bl2 -> Node "If Else" [toDataTree cond, toDataTree bl1, toDataTree bl2]
  NodeWhile cond bl -> Node "While" [toDataTree cond, toDataTree bl]
  NodeRead t -> Node "Read" [toDataTree t]
  NodeWrite t -> Node "Write" [toDataTree t]
  NodeAlloc t s -> Node "Alloc" [toDataTree t]
  NodeNew t s c -> Node ("New " ++ c ++ " ") [toDataTree t]
  NodeFree t -> Node "Free" [toDataTree t]
  _ -> Node (show t) []

prettyPrint :: SyntaxTree -> String
prettyPrint t = drawTree $ toDataTree t