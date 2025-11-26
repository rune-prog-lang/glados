module Rune.Semantics.Vars ( verifVars ) where

-- import Rune.AST.Visitor (RuneVisitor (..))
import Rune.AST.Nodes
import Data.Set (Set)
import qualified Data.Set as Set

type ScopeStack = Set String

-- if Nothing everything good else Error message
verifVars :: Program -> Maybe String
verifVars (Program _ defs) = foldMap verifDefs defs

verifDefs :: TopLevelDef -> Maybe String
verifDefs (DefFunction _ params _ body) = verifScope (Set.fromList (map paramName params)) body
verifDefs (DefOverride _ params _ body) = verifScope (Set.fromList (map paramName params)) body
verifDefs _ = Nothing


verifScope :: ScopeStack -> Block -> Maybe String
verifScope s ((StmtVarDecl name _ expr):stmts)
    = let s' = (Set.insert name s)
    in  verifExpr s expr
    <>  verifScope s' stmts
verifScope s ((StmtReturn expr):stmts)
    =   maybe Nothing (verifExpr s) expr
    <>  verifScope s stmts
verifScope s ((StmtIf cond thenB elseB):stmts)
    =   verifExpr s cond
    <>  verifScope s thenB
    <>  maybe Nothing (verifScope s) elseB
    <>  verifScope s stmts
verifScope s ((StmtFor var start end body):stmts)
    = let s' = Set.insert var s
    in  verifExpr s' start
    <>  verifExpr s' end
    <>  verifScope s' body
    <>  verifScope s stmts
verifScope s ((StmtForEach var iterable body):stmts)
    = let s' = Set.insert var s
    in  verifExpr s' iterable
    <>  verifScope s' body
    <>  verifScope s stmts
verifScope s ((StmtExpr expr):stmts) 
    =   verifExpr s expr
    <>  verifScope s stmts
verifScope _ [] = Nothing


verifExpr :: ScopeStack -> Expression -> Maybe String
verifExpr s (ExprBinary _ l r) = (verifExpr s l) <> (verifExpr s r)
verifExpr s (ExprCall _ args) = foldMap (verifExpr s) args
verifExpr s (ExprStructInit _ fields) = foldMap (verifExpr s . snd) fields
verifExpr s (ExprAccess target _) = verifExpr s target
verifExpr s (ExprUnary _ val) = verifExpr s val
verifExpr s (ExprVar var) =
    case Set.member var s of
        True -> Nothing
        False -> Just $ "\n\t" ++ var ++ " : var doesn't exist in the scope"
verifExpr _ _ = Nothing

