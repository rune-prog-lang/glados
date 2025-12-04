module Rune.IR.Generator.Expression.Call (genCall, genShowCall) where

import Rune.AST.Nodes (Expression)
import Rune.IR.IRHelpers (genFormatString, mangleOverrideName, newTemp, registerCall)
import Rune.IR.Nodes (IRGen, IRInstruction (..), IROperand (..), IRType (..))

--
-- callbacks to avoid circular dependencies
--

type GenExprCallback = Expression -> IRGen ([IRInstruction], IROperand, IRType)

--
-- public
--

genCall :: GenExprCallback -> String -> [Expression] -> IRGen ([IRInstruction], IROperand, IRType)
genCall genExpr funcName args = do
  argsData <- mapM genExpr args

  let argTypes = map (\(_, _, t) -> t) argsData
      overrideName = mangleOverrideName funcName argTypes
      mangled = mangleName funcName argsData overrideName
      (instrs, ops) = unzip $ map prepareArg argsData
      allInstrs = concat instrs

      -- TODO: improve return type inference:
      --  currently only handles struct and pointer-to-struct arguments
      --  otherwise defaults to IRI32
      retType = case argsData of
        ((_, _, IRStruct s) : _) -> IRStruct s
        ((_, _, IRPtr (IRStruct s)) : _) -> IRStruct s
        _ -> IRI32

  registerCall mangled
  retTemp <- newTemp "t" retType
  let callInstr = IRCALL retTemp mangled ops (Just retType)

  return (allInstrs ++ [callInstr], IRTemp retTemp retType, retType)

--
-- show calls
--

genShowCall :: GenExprCallback -> Expression -> IRGen ([IRInstruction], IROperand, IRType)
genShowCall genExpr arg = do
  (instrs, op, typ) <- genExpr arg
  let overrideName = mangleOverrideName "show" [typ]
      
      funcName = case getFormatSpecifier op typ of
        Just _ -> "printf"
        Nothing -> getShowFunc op typ overrideName
        
      (prep, finalOp) = prepareAddr op typ

  registerCall funcName
  (fmtInstrs, callArgs) <- genShowFmtCall op typ finalOp

  let callInstr = IRCALL "" funcName callArgs Nothing
  return (instrs ++ prep ++ fmtInstrs ++ [callInstr], IRTemp "t_null" IRNull, IRNull)

--
-- private
--

genShowFmtCall :: IROperand -> IRType -> IROperand -> IRGen ([IRInstruction], [IROperand])
genShowFmtCall originalOp typ finalOp = case getFormatSpecifier originalOp typ of
  Just fmt -> do
    (i, f) <- genFormatString fmt
    return (i, [f, finalOp])
  Nothing -> return ([], [finalOp])

getShowFunc :: IROperand -> IRType -> String -> String
getShowFunc _ (IRStruct s) _ = "show_" ++ s
getShowFunc _ (IRPtr (IRStruct s)) _ = "show_" ++ s
getShowFunc _ _ overrideName = overrideName

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
getFormatSpecifier _ IRBool = Just "%d"
getFormatSpecifier _ (IRPtr IRChar) = Just "%s"
getFormatSpecifier _ _ = Nothing

mangleName :: String -> [([IRInstruction], IROperand, IRType)] -> String -> String
mangleName base ((_, _, IRStruct s) : _) _ = s ++ "_" ++ base
mangleName base ((_, _, IRPtr (IRStruct s)) : _) _ = s ++ "_" ++ base
mangleName base _ _ = base

prepareArg :: ([IRInstruction], IROperand, IRType) -> ([IRInstruction], IROperand)
prepareArg (i, (IRTemp n t), IRStruct _) = (i ++ [IRADDR ("p_" ++ n) n (IRPtr t)], IRTemp ("p_" ++ n) (IRPtr t))
prepareArg (i, (IRTemp n t), IRPtr (IRStruct _)) = (i ++ [IRADDR ("p_" ++ n) n (IRPtr t)], IRTemp ("p_" ++ n) (IRPtr t))
prepareArg (i, op, _) = (i, op)

prepareAddr :: IROperand -> IRType -> ([IRInstruction], IROperand)
prepareAddr (IRTemp n _) (IRStruct t) =
  ( [IRADDR ("addr_" ++ n) n (IRPtr (IRStruct t))],
    IRTemp ("addr_" ++ n) (IRPtr (IRStruct t))
  )
prepareAddr (IRTemp n _) (IRPtr (IRStruct t)) =
  ( [IRADDR ("addr_" ++ n) n (IRPtr (IRPtr (IRStruct t)))],
    IRTemp ("addr_" ++ n) (IRPtr (IRPtr (IRStruct t)))
  )
prepareAddr op _ = ([], op)
