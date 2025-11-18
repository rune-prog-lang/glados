{-
-- EPITECH PROJECT, 2025
-- AST.hs
-- File description:
-- AST.hs
-}

module AST (
    Ast(..),
    sexprToAST,
    handleCall,
    handleString,
    evalAST
) where

import SExpr (SExpr(..))

data Ast = Define Ast Ast
    | Call String [Ast]
    | Lambda [Ast] Ast
    | If Ast Ast Ast
    | AstInteger Int
    | AstFloat Float
    | AstSymbol String
    | AstBoolean Bool
    | AstList [Ast]
    deriving (Show, Eq)

sexprToAST :: SExpr -> Maybe Ast
sexprToAST (Integer n) = Just (AstInteger n)
sexprToAST (Symbol s) = Just (AstSymbol s)
sexprToAST (List [Symbol "define", varName, valueExpr]) = do
    astValue <- sexprToAST valueExpr
    astVarName <- sexprToAST varName
    Just (Define astVarName astValue)
sexprToAST (List (Symbol "define" : _)) = Nothing
sexprToAST (List [Symbol "lambda", List args, body]) = do
    astValue <- sexprToAST body
    astArgs <- mapM sexprToAST args
    Just (Lambda astArgs astValue)
sexprToAST (List (Symbol "lambda" : _)) = Nothing
sexprToAST (List [Symbol "if", condExpr, thenExpr, elseExpr]) = do
    astCond <- sexprToAST condExpr
    astThen <- sexprToAST thenExpr
    astElse <- sexprToAST elseExpr
    Just (If astCond astThen astElse)
sexprToAST (List (funcExpr:argExprs)) = do
    astFunc <- sexprToAST funcExpr
    astArgs <- mapM sexprToAST argExprs
    case astFunc of
        AstSymbol op | op `elem` ["+", "-", "*", "div", "mod", "eq?", "<"] ->
            Just (Call op astArgs)
        _ -> Just (AstList (astFunc:astArgs))
sexprToAST (List exprs) = do
    astExprs <- mapM sexprToAST exprs
    Just (AstList astExprs)

-----------------------------------------------------------------------------------------------
-- Evaluation of AST
-----------------------------------------------------------------------------------------------

type Environment = [(Ast, Ast)]

compAst :: Ast -> Ast -> Bool
compAst (AstInteger a) (AstInteger b) = a == b
compAst (AstSymbol a) (AstSymbol b)   = a == b
compAst (AstBoolean a) (AstBoolean b) = a == b
compAst (List a) (List b)               = and $ zipWith compAst a b
compAst _ _                           = False

extractInteger :: Ast -> Maybe Int
extractInteger ast = case evalAST ast of
    Just (AstInteger val) -> Just val
    _                     -> Nothing

handleString :: String -> Ast
handleString "#t" = AstBoolean True
handleString "#f" = AstBoolean False
handleString s = AstSymbol s

handleCall :: Environment -> String -> [Ast] -> Maybe Ast
handleCall env op (x:y:_) | op `elem` ["+", "-", "*", "div", "mod", "eq?", "<"] = do
    a <- extractInteger x
    b <- extractInteger y
    case op of
        "+" -> Just (AstInteger (a + b))
        "-" -> Just (AstInteger (a - b))
        "*" -> Just (AstInteger (a * b))
        "div" -> if b /= 0 then Just (AstInteger (a `div` b)) else Nothing
        "mod" -> if b /= 0 then Just (AstInteger (a `mod` b)) else Nothing
        "eq?" -> Just (AstBoolean (a == b))
        "<" -> Just (AstBoolean (a < b))
        _   -> Nothing
handleCall _ _ = Nothing

handleCondition :: Ast -> Ast -> Ast -> Maybe Ast
handleCondition c t e = do
        evaluatedCond <- evalAST c
        case evaluatedCond of
            AstBoolean True  -> evalAST t
            AstBoolean False -> evalAST e
            _                -> Nothing

evalAST :: Environment -> Ast -> Maybe Ast
evalAST env (Define varName value) = do
    evaluatedValue <- evalAST value
    Just (Define varName evaluatedValue)
evalAST env (Call func args) = handleCall func args
evalAST env (AstInteger n) = Just (AstInteger n)
evalAST env (AstFloat f)   = Just (AstFloat f)
evalAST env (AstSymbol s)  = Just (handleString s)
evalAST env (AstBoolean b) = Just (AstBoolean b)
evalAST env (If cond thenExpr elseExpr) = handleCondition cond thenExpr elseExpr
evalAST env (AstList exprs) = do
    astExprs <- mapM evalAST exprs
    Just (AstList astExprs)
