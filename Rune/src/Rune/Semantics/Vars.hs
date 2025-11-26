module Rune.Semantics.Vars ( verifVars ) where

import Rune.AST.Nodes
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as HM

type VarStack = HashMap String Type

-- if Nothing everything good else Error message
verifVars :: Program -> Maybe String
verifVars (Program _ defs) = foldMap verifDefs defs

verifDefs :: TopLevelDef -> Maybe String
verifDefs (DefFunction _ params _ body) = verifScope (HM.fromList (map (\p -> (paramName p, paramType p)) params)) body
verifDefs (DefOverride _ params _ body) = verifScope (HM.fromList (map (\p -> (paramName p, paramType p)) params)) body
-- verifDefs (DefStruct _ attr _) = -- when handling struct each var of type struct can expand to var.attr
verifDefs _ = Nothing


verifScope :: VarStack -> Block -> Maybe String
-- name n, type t, expr e
verifScope s ((StmtVarDecl n t e):stmts)
    = let s' = (HM.insert n (fromMaybe TypeAny t) s)
    in  verifExpr s e
    <>  verifScope s' stmts
verifScope s ((StmtReturn e):stmts)
    =   maybe Nothing (verifExpr s) e
    <>  verifScope s stmts
verifScope s ((StmtIf cond thenA elseB):stmts)
    =   verifExpr s cond
    <>  verifScope s thenA
    <>  maybe Nothing (verifScope s) elseB
    <>  verifScope s stmts
verifScope s ((StmtFor var start end body):stmts)
    = let s' = HM.insert var TypeAny s
    in  verifExpr s' start
    <>  verifExpr s' end
    <>  verifScope s' body
    <>  verifScope s stmts
verifScope s ((StmtForEach var iterable body):stmts)
    = let s' = HM.insert var TypeAny s
    in  verifExpr s' iterable
    <>  verifScope s' body
    <>  verifScope s stmts
verifScope s ((StmtExpr e):stmts) 
    =   verifExpr s e
    <>  verifScope s stmts
verifScope _ [] = Nothing


verifExpr :: VarStack -> Expression -> Maybe String
verifExpr s (ExprBinary _ l r) = (verifExpr s l) <> (verifExpr s r)
verifExpr s (ExprCall _ args) = foldMap (verifExpr s) args
verifExpr s (ExprStructInit _ fields) = foldMap (verifExpr s . snd) fields
verifExpr s (ExprAccess target _) = verifExpr s target
verifExpr s (ExprUnary _ val) = verifExpr s val
verifExpr s (ExprVar var) =
    case HM.member var s of
        True -> Nothing
        False -> Just $ "\n\t" ++ var ++ " : var doesn't exist in the scope"
verifExpr _ _ = Nothing
