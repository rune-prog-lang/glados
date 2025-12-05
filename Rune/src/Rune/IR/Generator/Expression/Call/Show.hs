module Rune.IR.Generator.Expression.Call.Show (genShowCall) where

import Rune.AST.Nodes (Expression)
import Rune.IR.IRHelpers (genFormatString, registerCall)
import Rune.IR.Nodes (IRGen, IRInstruction (..), IROperand (..), IRType (..))

--
-- callbacks to avoid circular dependencies
--

type GenExprCallback = Expression -> IRGen ([IRInstruction], IROperand, IRType)

--
-- show calls
-- show is a built-in function that prints the value of its argument to stdout
-- def show(value: any) -> null
--

--
-- public
--

genShowCall :: GenExprCallback -> Expression -> IRGen ([IRInstruction], IROperand, IRType)
genShowCall genExpr arg = do
  (instrs, op, typ) <- genExpr arg
  let funcName = getShowFunc op typ
      (prep, finalOp) = prepareAddr op typ

  registerCall funcName
  (fmtInstrs, callArgs) <- genShowFmtCall op typ finalOp

  let callInstr = IRCALL "" funcName callArgs Nothing
  return (instrs ++ prep ++ fmtInstrs ++ [callInstr], IRTemp "t_null" IRNull, IRNull)

--
-- private
--

-- | as show is a built-in "printf"-like function
-- def show(value: any) -> null
-- we need to format the arguments accordingly to the input
genShowFmtCall :: IROperand -> IRType -> IROperand -> IRGen ([IRInstruction], [IROperand])
genShowFmtCall originalOp typ finalOp = case getFormatSpecifier originalOp typ of
  Just fmt -> do
    (i, f) <- genFormatString fmt
    return (i, [f, finalOp])
  Nothing -> return ([], [finalOp])

getShowFunc :: IROperand -> IRType -> String
getShowFunc _ (IRStruct s) = "show_" ++ s
getShowFunc _ (IRPtr (IRStruct s)) = "show_" ++ s
getShowFunc _ _ = "printf"

getFormatSpecifier :: IROperand -> IRType -> Maybe String
getFormatSpecifier _ IRI8 = Just "%hhd"
getFormatSpecifier _ IRI16 = Just "%hd"
getFormatSpecifier _ IRI32 = Just "%d"
getFormatSpecifier _ IRI64 = Just "%ld"
getFormatSpecifier _ IRU8 = Just "%hhu"
getFormatSpecifier _ IRU16 = Just "%hu"
getFormatSpecifier _ IRU32 = Just "%u"
getFormatSpecifier _ IRU64 = Just "%lu"
getFormatSpecifier _ IRChar = Just "%c"
getFormatSpecifier _ IRF32 = Just "%f"
getFormatSpecifier _ IRF64 = Just "%lf"
getFormatSpecifier _ IRBool = Just "%d" -- should be 1 ? "true" : "false"
getFormatSpecifier _ IRNull = Just "(null)"
getFormatSpecifier _ (IRPtr IRChar) = Just "%s"
getFormatSpecifier _ _ = Nothing

prepareAddr :: IROperand -> IRType -> ([IRInstruction], IROperand)
prepareAddr (IRTemp n _) (IRStruct t) =
  ( [IRADDR ("addr_" ++ n) n (IRPtr (IRStruct t))]
  , IRTemp ("addr_" ++ n) (IRPtr (IRStruct t))
  )
prepareAddr (IRTemp n _) (IRPtr (IRStruct t)) =
  ( [IRADDR ("addr_" ++ n) n (IRPtr (IRPtr (IRStruct t)))]
  , IRTemp ("addr_" ++ n) (IRPtr (IRPtr (IRStruct t)))
  )
prepareAddr op _ = ([], op)
