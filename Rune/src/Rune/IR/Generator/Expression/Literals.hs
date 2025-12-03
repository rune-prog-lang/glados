module Rune.IR.Generator.Expression.Literals
  ( genLitInt,
    genLitFloat,
    genLitChar,
    genLitBool,
    genLitNull,
    genLitString,
  )
where

import Rune.IR.IRHelpers (newStringGlobal, newTemp)
import Rune.IR.Nodes
  ( IRGen,
    IRInstruction (..),
    IROperand (..),
    IRType (..),
  )

--
-- public
--

genLitInt :: Int -> IRGen ([IRInstruction], IROperand, IRType)
genLitInt n = return ([], IRConstInt n, IRI32)

genLitFloat :: Double -> IRGen ([IRInstruction], IROperand, IRType)
genLitFloat f = return ([], IRConstFloat f, IRF32)

genLitChar :: Char -> IRGen ([IRInstruction], IROperand, IRType)
genLitChar c = return ([], IRConstChar c, IRChar)

genLitBool :: Bool -> IRGen ([IRInstruction], IROperand, IRType)
genLitBool True = return ([], IRConstInt 1, IRI32)
genLitBool False = return ([], IRConstInt 0, IRI32)

genLitNull :: IRGen ([IRInstruction], IROperand, IRType)
genLitNull = return ([], IRConstInt 0, IRNull)

genLitString :: String -> IRGen ([IRInstruction], IROperand, IRType)
genLitString s = do
  stringName <- newStringGlobal s
  ptrName <- newTemp "p_ptr" (IRPtr IRChar)
  let addrInstr = IRADDR ptrName stringName (IRPtr IRChar)
  return ([addrInstr], IRTemp ptrName (IRPtr IRChar), IRPtr IRChar)
