module Rune.Semantics.Vars ( verifVars ) where

import Rune.AST.Nodes
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as HM
import Rune.Semantics.Func (findFunc, FuncStack)

type VarStack = HashMap String Type
type Stack = (FuncStack, VarStack)

-- if Nothing everything good else Error message
verifVars :: Program -> Maybe String
verifVars (Program n defs) =
    foldMap (verifDefs (findFunc (Program n defs))) defs

verifDefs :: FuncStack -> TopLevelDef -> Maybe String
verifDefs fs (DefFunction _ params _ body) = verifScope (fs, HM.fromList (map (\p -> (paramName p, paramType p)) params)) body
verifDefs fs (DefOverride _ params _ body) = verifScope (fs, HM.fromList (map (\p -> (paramName p, paramType p)) params)) body
-- verifDefs fs (DefStruct _ attr _) = undefined -- when there will be type and all
verifDefs _ _ = Nothing


verifScope :: Stack -> Block -> Maybe String
-- name n, type t, expr e
verifScope (fs, vs) ((StmtVarDecl n t e):stmts)
    = let vs' = (HM.insert n (fromMaybe TypeAny t) vs)
    in  verifExpr (fs, vs) e
    <>  verifScope (fs, vs') stmts
verifScope s ((StmtReturn e):stmts)
    =   maybe Nothing (verifExpr s) e
    <>  verifScope s stmts
verifScope s ((StmtIf cond thenA elseB):stmts)
    =   verifExpr s cond
    <>  verifScope s thenA
    <>  maybe Nothing (verifScope s) elseB
    <>  verifScope s stmts
verifScope (fs, vs) ((StmtFor var start end body):stmts)
    = let vs' = HM.insert var TypeAny vs
    in  verifExpr (fs, vs') start
    <>  verifExpr (fs, vs') end
    <>  verifScope (fs, vs') body
    <>  verifScope (fs, vs) stmts
verifScope (fs, vs) ((StmtForEach var iterable body):stmts)
    = let vs' = HM.insert var TypeAny vs
    in  verifExpr (fs, vs') iterable
    <>  verifScope (fs, vs') body
    <>  verifScope (fs, vs) stmts
verifScope s ((StmtExpr e):stmts) 
    =   verifExpr s e
    <>  verifScope s stmts
verifScope _ [] = Nothing


verifExpr :: Stack -> Expression -> Maybe String
verifExpr s (ExprBinary _ l r) = (verifExpr s l) <> (verifExpr s r)
verifExpr s (ExprCall _ args) = foldMap (verifExpr s) args
verifExpr s (ExprStructInit _ fields) = foldMap (verifExpr s . snd) fields
verifExpr s (ExprAccess target _) = verifExpr s target
verifExpr s (ExprUnary _ val) = verifExpr s val
verifExpr s (ExprVar var) =
    case HM.member var (snd s) of
        True -> Nothing
        False -> Just $ "\n\t" ++ var ++ " : var doesn't exist in the scope"
verifExpr _ _ = Nothing
