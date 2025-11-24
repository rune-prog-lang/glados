{-
-- EPITECH PROJECT, 2025
-- Executor.hs
-- File description:
-- Executor.hs
-}

module Executor (
    executeLisp
) where

import AST (Ast(..), evalAST, sexprToAST)
import SExpr (SExpr)
import Parser (parseLispDocument)
import Text.Parsec (parse)

executeLisp :: String -> Maybe Ast
executeLisp input = case parse parseLispDocument "" input of
        Left _ -> Nothing
        Right sexpr -> do
            ast <- sexprToAST sexpr
            evalAST [] ast
