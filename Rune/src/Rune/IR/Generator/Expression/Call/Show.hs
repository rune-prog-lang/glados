module Rune.IR.Generator.Expression.Call.Show (genShowCall) where

import Rune.AST.Nodes (Expression)
import Rune.IR.IRHelpers (genFormatString, makeLabel, newTemp, nextLabelIndex, registerCall)
import Rune.IR.Nodes (IRGen, IRInstruction (..), IRLabel (..), IROperand (..), IRType (..))

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
genShowFmtCall _ IRBool boolOp = genBoolShowCall boolOp
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
getFormatSpecifier _ IRBool = Nothing
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

--
-- boolean show helpers
--

genBoolShowCall :: IROperand -> IRGen ([IRInstruction], [IROperand])
genBoolShowCall boolOp = do
  (convInstrs, strOp) <- genBoolToStringConversion boolOp
  (fmtInstrs, fmtOp) <- genFormatString "%s"
  return (convInstrs ++ fmtInstrs, [fmtOp, strOp])

genBoolToStringConversion :: IROperand -> IRGen ([IRInstruction], IROperand)
genBoolToStringConversion boolOp = do
  (litInstrs, trueStr, falseStr) <- genBoolStringLiterals
  resultTemp <- newTemp "bool_str" (IRPtr IRChar)
  labels <- genBoolConversionLabels
  let convInstrs = buildBoolConversionInstrs boolOp trueStr falseStr resultTemp labels
  return (litInstrs ++ convInstrs, IRTemp resultTemp (IRPtr IRChar))

genBoolStringLiterals :: IRGen ([IRInstruction], IROperand, IROperand)
genBoolStringLiterals = do
  (trueInstrs, trueOp) <- genFormatString "(true)"
  (falseInstrs, falseOp) <- genFormatString "(false)"
  return (trueInstrs ++ falseInstrs, trueOp, falseOp)

genBoolConversionLabels :: IRGen (IRLabel, IRLabel, IRLabel)
genBoolConversionLabels = do
  trueIdx <- nextLabelIndex
  falseIdx <- nextLabelIndex
  endIdx <- nextLabelIndex
  return
    ( makeLabel "bool_true_" trueIdx,
      makeLabel "bool_false_" falseIdx,
      makeLabel "bool_end_" endIdx
    )

buildBoolConversionInstrs :: IROperand -> IROperand -> IROperand -> String -> (IRLabel, IRLabel, IRLabel) -> [IRInstruction]
buildBoolConversionInstrs boolOp trueStr falseStr resultTemp (trueLabel, falseLabel, endLabel) =
  [ IRJUMP_TRUE boolOp trueLabel,
    IRJUMP falseLabel,
    IRLABEL trueLabel,
    IRASSIGN resultTemp trueStr (IRPtr IRChar),
    IRJUMP endLabel,
    IRLABEL falseLabel,
    IRASSIGN resultTemp falseStr (IRPtr IRChar),
    IRLABEL endLabel
  ]

