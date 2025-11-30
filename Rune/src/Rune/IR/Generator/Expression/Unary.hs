module Rune.IR.Generator.Expression.Unary
  ( genUnary,
  )
where

import Rune.AST.Nodes (Expression, UnaryOp (..))
import Rune.IR.IRHelpers (newTemp)
import Rune.IR.Nodes
  ( IRGen,
    IRInstruction (..),
    IROperand (..),
    IRType (..),
  )

--
-- callbacks to avoid circular dependencies
--

type GenExprCallback = Expression -> IRGen ([IRInstruction], IROperand, IRType)

--
-- public
--

genUnary :: GenExprCallback -> UnaryOp -> Expression -> IRGen ([IRInstruction], IROperand, IRType)
genUnary genExpr op expr = do
  (instrs, operand, typ) <- genExpr expr
  case op of
    Negate -> do
      resultTemp <- newTemp "t" typ
      let negInstr = IRSUB_OP resultTemp (IRConstInt 0) operand typ
      return (instrs ++ [negInstr], IRTemp resultTemp typ, typ)
    PrefixInc -> return (instrs ++ [IRINC operand], operand, typ)
    PrefixDec -> return (instrs ++ [IRDEC operand], operand, typ)
    PostfixInc -> do
      resultTemp <- newTemp "t" typ
      return (instrs ++ [IRASSIGN resultTemp operand typ, IRINC operand], IRTemp resultTemp typ, typ)
    PostfixDec -> do
      resultTemp <- newTemp "t" typ
      return (instrs ++ [IRASSIGN resultTemp operand typ, IRDEC operand], IRTemp resultTemp typ, typ)
    PropagateError -> return (instrs, operand, typ)
