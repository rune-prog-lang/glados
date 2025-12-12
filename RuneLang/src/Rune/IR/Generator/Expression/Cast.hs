{-# OPTIONS_GHC -cpp #-}

#if defined(TESTING_EXPORT)
module Rune.IR.Generator.Expression.Cast
  ( genCast,
    applyCast
  )
where
#else
module Rune.IR.Generator.Expression.Cast (genCast) where
#endif

import Rune.AST.Nodes (Expression (..), Type (..))
import Rune.IR.IRHelpers (astTypeToIRType, newTemp)
import Rune.IR.Nodes (IRGen, IRInstruction (..), IROperand (..), IRType (..))

type IRGenResult = IRGen ([IRInstruction], IROperand, IRType)
type IRGenCast = Expression -> Type -> IRGenResult

--
-- public
--

genCast :: (Expression -> IRGen ([IRInstruction], IROperand, IRType)) -> IRGenCast
genCast genExpr e t = genExpr e >>= applyCast (astTypeToIRType t)

--
-- private
--

applyCast :: IRType -> ([IRInstruction], IROperand, IRType) -> IRGenResult
applyCast destType (instrs, op, srcType)
  | srcType == destType = return (instrs, op, destType)
  | otherwise = do
      tempName <- newTemp "cast" destType
      let castInstr = IRCAST tempName op destType
      return (instrs ++ [castInstr], IRTemp tempName destType, destType)
