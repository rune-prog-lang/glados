module Executor (
    executeLispWithEnv,
    astToString
) where

import Lisp.AST.AST (Ast(..), evalAST, evalASTWithEnv, Environment)
import Lisp.AST.SExprToAST (sexprToAST)
import Lisp.Parser.Parser (parseLispDocument)
import Lisp.SExpr.SExpr (SExpr(..))
import Text.Megaparsec (parse, errorBundlePretty)

executeLispWithEnv :: Environment -> String -> (Environment, Either String Ast)
executeLispWithEnv env input =
    case parse parseLispDocument "" input of
        Left err -> (env, Left $ "Parse error: " ++ errorBundlePretty err)
        Right sexpr -> executeSExpr env sexpr

executeSExpr :: Environment -> SExpr -> (Environment, Either String Ast)
executeSExpr env (List sexprs) =
    case mapM sexprToAST sexprs of
        Left err -> (env, Left ("AST conversion error: " ++ err))
        Right asts ->
            let (newEnv, result) = evalASTWithEnv env asts
            in (newEnv, addEvalErrorPrefix result)
executeSExpr env sexpr =
    case sexprToAST sexpr of
        Left err -> (env, Left ("AST conversion error: " ++ err))
        Right ast ->
            let (newEnv, result) = evalAST env ast
            in (newEnv, addEvalErrorPrefix result)

addEvalErrorPrefix :: Either String Ast -> Either String Ast
addEvalErrorPrefix (Left err) = Left ("Evaluation error: " ++ err)
addEvalErrorPrefix (Right ast) = Right ast

astToString :: Ast -> String
astToString (AstInteger n) = show n
astToString (AstBoolean True) = "#t"
astToString (AstBoolean False) = "#f"
astToString (AstSymbol s) = s
astToString (Lambda _ _ _) = "#<procedure>"
astToString _ = ""
