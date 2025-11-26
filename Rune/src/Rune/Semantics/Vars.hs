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

-- visitStatement (StmtVarDecl name typeDecl expr) = visitVarDecl name typeDecl expr
-- visitStatement (StmtReturn expr) = visitReturn expr
-- visitStatement (StmtIf cond thenB elseB) = visitIf cond thenB elseB
-- visitStatement (StmtFor var start end body) = visitFor var start end body
-- visitStatement (StmtForEach var iterable body) = visitForEach var iterable body
-- visitStatement (StmtExpr expr) = void $ visitExpression expr
verifScope :: ScopeStack -> Block -> Maybe String
verifScope scope ((StmtVarDecl name _ expr):stmts) = 
    verifExpr scope expr <> verifScope (Set.insert name scope) stmts
verifScope _ _ = Nothing

-- visitExpression (ExprBinary _ l r) = visitExpression l >> visitExpression r
-- visitExpression (ExprUnary _ val) = visitExpression val
-- visitExpression (ExprCall _ args) = mapM_ visitExpression args
-- visitExpression (ExprStructInit _ fields) = mapM_ (visitExpression . snd) fields
-- visitExpression (ExprAccess target _) = visitExpression target
-- visitExpression _ = return ()
verifExpr :: ScopeStack -> Expression -> Maybe String
verifExpr s (ExprBinary _ l r) = (verifExpr s l) <> (verifExpr s r)
verifExpr s (ExprCall _ args) = foldMap (verifExpr s) args
verifExpr s (ExprStructInit _ fields) = foldMap (verifExpr s . snd) fields
verifExpr s (ExprAccess target _) = verifExpr s target
verifExpr s (ExprUnary _ val) = verifExpr s val
verifExpr s (ExprVar var) =
    case Set.member var s of
        True -> Nothing
        False -> Just $ "\n\t" ++ var ++ " : doesn't exist in the scope"
verifExpr _ _ = Nothing

