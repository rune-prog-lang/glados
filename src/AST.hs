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
sexprToAST (List (Symbol "+": args)) = do
    astArgs <- mapM sexprToAST args
    Just (Call "+" astArgs)
sexprToAST (List (Symbol "<": args)) = do
    astArgs <- mapM sexprToAST args
    Just (Call "<" astArgs)
sexprToAST (List (Symbol "-": args)) = do
    astArgs <- mapM sexprToAST args
    Just (Call "-" astArgs)
sexprToAST (List (Symbol "*": args)) = do
    astArgs <- mapM sexprToAST args
    Just (Call "*" astArgs)
sexprToAST (List (Symbol "div": args)) = do
    astArgs <- mapM sexprToAST args
    Just (Call "div" astArgs)
sexprToAST (List (Symbol "eq?": args)) = do
    astArgs <- mapM sexprToAST args
    Just (Call "eq?" astArgs)
sexprToAST (List (Symbol "mod": args)) = do
    astArgs <- mapM sexprToAST args
    Just (Call "mod" astArgs)
sexprToAST (List exprs) = do
    astExprs <- mapM sexprToAST exprs
    Just (AstList astExprs)

-----------------------------------------------------------------------------------------------
-- Evaluation of AST
-----------------------------------------------------------------------------------------------

extractInteger :: Ast -> Maybe Int
extractInteger ast = case evalAST ast of
    Just (AstInteger val) -> Just val
    _                     -> Nothing

handleString :: String -> Ast
handleString "#t" = AstBoolean True
handleString "#f" = AstBoolean False
handleString s = AstSymbol s

handleCall :: String -> [Ast] -> Maybe Ast
handleCall op (x:y:_) | op `elem` ["+", "-", "*", "div", "mod", "eq?", "<"] = do
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

evalAST :: Ast -> Maybe Ast
evalAST (Define varName value) = do
    evaluatedValue <- evalAST value
    Just (Define varName evaluatedValue)
evalAST (Call func args) = handleCall func args
evalAST (AstInteger n) = Just (AstInteger n)
evalAST (AstFloat f)   = Just (AstFloat f)
evalAST (AstSymbol s)  = Just (handleString s)
evalAST (AstBoolean b) = Just (AstBoolean b)
evalAST (If cond thenExpr elseExpr) = handleCondition cond thenExpr elseExpr
evalAST (AstList exprs) = do
    astExprs <- mapM evalAST exprs
    Just (AstList astExprs)
