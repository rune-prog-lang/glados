module Rune.IR.Generator.GenExpression (genExpression) where

import Control.Monad.State (gets)
import qualified Data.Map.Strict as Map
import Rune.AST.Nodes (Expression (..))
import Rune.IR.Generator.Expression.Binary (genBinary)
import Rune.IR.Generator.Expression.Call (genCall, genShowCall)
import Rune.IR.Generator.Expression.Literals
import Rune.IR.Generator.Expression.Struct (genAccess, genStructInit)
import Rune.IR.Generator.Expression.Unary (genUnary)
import Rune.IR.Nodes (GenState (..), IRGen, IRInstruction (..), IROperand (..), IRType (..))

--
-- public
--

genExpression :: Expression -> IRGen ([IRInstruction], IROperand, IRType)
genExpression expr = case expr of
  ExprLitInt n -> genLitInt n
  ExprLitFloat f -> genLitFloat f
  ExprLitChar c -> genLitChar c
  ExprLitBool b -> genLitBool b
  ExprLitNull -> genLitNull
  ExprLitString s -> genLitString s
  ExprVar name -> genVar name
  ExprBinary op l r -> genBinary genExpression op l r
  ExprUnary op e -> genUnary genExpression op e
  ExprCall "show" [a] -> genShowCall genExpression a
  ExprCall name args -> genCall genExpression name args
  ExprAccess t f -> genAccess genExpression t f
  ExprStructInit name fields -> genStructInit genExpression name fields

--
-- private
--

genVar :: String -> IRGen ([IRInstruction], IROperand, IRType)
genVar name = do
  symTable <- gets gsSymTable
  case Map.lookup name symTable of
    Just (op, typ) -> return ([], op, typ)
    Nothing -> error $ "genVar: variable not found in symbol table: " ++ name
