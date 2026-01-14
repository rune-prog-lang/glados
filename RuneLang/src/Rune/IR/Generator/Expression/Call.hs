{-# LANGUAGE CPP #-}

#if defined(TESTING_EXPORT)
module Rune.IR.Generator.Expression.Call
  ( genCall,
    prepareArg,
    genArgWithContext
  )
where
#else
module Rune.IR.Generator.Expression.Call (genCall) where
#endif

import Control.Monad (zipWithM)
import Control.Monad.State (gets)
import Control.Monad.Except (throwError)
import qualified Data.HashMap.Strict as HM
import Data.List (find)
import Rune.AST.Nodes (Expression, Type(..), Parameter(..), paramType)
import Rune.IR.IRHelpers (registerCall, newTemp, astTypeToIRType, isFloatType)
import Rune.IR.Nodes (GenState(..), IRGen, IRInstruction (..), IROperand (..), IRType (..))

--
-- callbacks to avoid circular dependencies
--

type GenExprCallback = Expression -> IRGen ([IRInstruction], IROperand, IRType)

--
-- public
--

genCall :: GenExprCallback -> String -> [Expression] -> IRGen ([IRInstruction], IROperand, IRType)
genCall genExpr funcName args = do
  fs <- gets gsFuncStack
  
  -- Find all overloads matching this base name
  let allFuncs = HM.toList fs
      -- Find functions that match either exactly or by base name extraction
      matchingFuncs = filter (\(name, _) -> matchesBaseName funcName name) allFuncs
      
      -- Find a variadic overload
      variadicOverload = find (\(_, (_, params)) -> hasVariadicParam params) matchingFuncs
      
      -- Count non-variadic single-arg overloads (these are the dispatch targets)
      singleArgOverloads = filter (\(_, (_, params)) -> 
        length params == 1 && not (hasVariadicParam params)) matchingFuncs
      
      -- Only unroll if we have dispatch targets (single-arg overloads to call)
      shouldUnroll = length singleArgOverloads > 0 && length matchingFuncs > 1
  
  -- If we have more args than any non-variadic overload can handle AND there's a variadic overload
  -- AND we have single-arg overloads to dispatch to, then we unroll
  case variadicOverload of
    Just (_, (retType, params)) | shouldUnroll && length args > maxNonVariadicParams matchingFuncs -> do
      -- Variadic call that needs unrolling
      let normalParams = takeWhile (not . isVariadicParam . paramType) params
          numNormalArgs = length normalParams
          variadicArgs = drop numNormalArgs args
      
      -- Generate all variadic arguments
      variadicArgsData <- mapM genExpr variadicArgs
      
      let irRetType = case retType of
                        TypeArray elemType -> IRPtr (IRArray (astTypeToIRType elemType) 0)
                        t -> astTypeToIRType t
      
      -- For each variadic argument, find the right overload based on type and call it
      (allInstrs, resultOps) <- unzip <$> mapM (genOverloadedCall genExpr funcName matchingFuncs irRetType) variadicArgsData
      
      -- Accumulate results
      case resultOps of
        [] -> throwError "No variadic arguments provided"
        [single] -> pure (concat allInstrs, single, irRetType)
        _ -> do
          (accInstrs, finalResult) <- accumulateResults irRetType resultOps
          pure (concat allInstrs ++ accInstrs, finalResult, irRetType)
    
    -- Normal function call (no unrolling - either no variadic overload, no dispatch targets, or not enough args)
    _ -> do
      let funcSignature = HM.lookup funcName fs
      
      argsData <- case funcSignature of
        Just (_, params)
          | length params == length args ->
              zipWithM (genArgWithContext genExpr) args (map paramType params)
          | hasVariadicParam params -> do
              -- Handle variadic args: normal args + remaining args for variadic param
              let normalParams = takeWhile (not . isVariadicParam . paramType) params
                  numNormalArgs = length normalParams
                  (normalArgs, variadicArgs) = splitAt numNormalArgs args
              normalArgsData <- zipWithM (genArgWithContext genExpr) normalArgs (map paramType normalParams)
              variadicArgsData <- mapM genExpr variadicArgs
              pure $ normalArgsData ++ variadicArgsData
        _ -> mapM genExpr args

      let (instrs, ops) = unzip $ map prepareArg argsData
          allInstrs     = concat instrs

      retType <- case funcSignature of
        Just (rt, _) -> pure $ case rt of
                                 TypeArray elemType -> IRPtr (IRArray (astTypeToIRType elemType) 0)
                                 t -> astTypeToIRType t
        Nothing -> throwError $ "IR error: Function " <> funcName <> " not found in function stack"

      registerCall funcName
      retTemp <- newTemp "t" retType

      let callInstr = IRCALL retTemp funcName ops (Just retType)

      pure (allInstrs <> [callInstr], IRTemp retTemp retType, retType)
  where
    matchesBaseName :: String -> String -> Bool
    matchesBaseName base full = 
      base == full || 
      extractBaseName full == base ||
      ("_" ++ base ++ "_") `isInfixOf'` ("_" ++ full ++ "_")
      where
        isInfixOf' needle haystack = any (needle `isPrefixOf'`) (tails' haystack)
        isPrefixOf' prefix str = take (length prefix) str == prefix
        tails' [] = [[]]
        tails' xs@(_:rest) = xs : tails' rest
    
    extractBaseName :: String -> String
    extractBaseName name = 
      -- Pattern: retType_baseName_argTypes... or just baseName
      case break (== '_') name of
        (_, "") -> name  -- No underscore, return as is
        (_, rest) -> 
          case break (== '_') (drop 1 rest) of
            (baseName, _) -> baseName
    
    maxNonVariadicParams :: [(String, (Type, [Parameter]))] -> Int
    maxNonVariadicParams funcs = 
      maximum (0 : [length params | (_, (_, params)) <- funcs, not (hasVariadicParam params)])
    
    hasVariadicParam :: [Parameter] -> Bool
    hasVariadicParam = any (isVariadicParam . paramType)
    
    isVariadicParam :: Type -> Bool
    isVariadicParam (TypeVariadic _) = True
    isVariadicParam _ = False
    
    -- Find the best matching overload for an argument based on its type
    genOverloadedCall :: GenExprCallback -> String -> [(String, (Type, [Parameter]))] -> IRType -> ([IRInstruction], IROperand, IRType) -> IRGen ([IRInstruction], IROperand)
    genOverloadedCall _ baseName overloads defaultRetType (argInstrs, argOp, argType) = do
      -- Find best matching overload for this argument type
      let singleArgOverloads = filter (\(_, (_, params)) -> 
            length params == 1 && not (hasVariadicParam params)) overloads
          
          -- Convert IRType to Type for matching
          argASTType = irTypeToASTType argType
          
          -- Extract inner type from TypeRef for matching
          unwrapRef :: Type -> Type
          unwrapRef (TypeRef t) = t
          unwrapRef t = t
          
          -- Check if param is a reference
          isRefParam :: Type -> Bool
          isRefParam (TypeRef _) = True
          isRefParam _ = False
          
          -- Find exact match first (unwrapping refs)
          exactMatch = find (\(_, (_, params)) -> 
            let pType = unwrapRef (paramType (head' params))
            in pType == argASTType) singleArgOverloads
          
          -- Find compatible match
          compatibleMatch = find (\(_, (_, params)) -> 
            let pType = unwrapRef (paramType (head' params))
            in isTypeCompatible' pType argASTType) singleArgOverloads
          
          bestMatch = exactMatch `orElse` compatibleMatch
      
      case bestMatch of
        Just (matchedName, (retType, params)) -> do
          let irRetType = astTypeToIRType retType
              paramType' = paramType (head' params)
              needsRef = isRefParam paramType'
          
          -- If the parameter is a reference type, we need to pass the address
          (finalInstrs, finalOp) <- if needsRef
            then case argOp of
              IRGlobal name _ -> do
                -- For globals, use IRADDR to get address
                addrTemp <- newTemp "addr" (IRPtr argType)
                let addrInstr = IRADDR addrTemp name (IRPtr argType)
                pure (argInstrs ++ [addrInstr], IRTemp addrTemp (IRPtr argType))
              IRTemp name _ -> do
                -- For temps, use IRADDR to get address
                addrTemp <- newTemp "addr" (IRPtr argType)
                let addrInstr = IRADDR addrTemp name (IRPtr argType)
                pure (argInstrs ++ [addrInstr], IRTemp addrTemp (IRPtr argType))
              _ -> do
                -- For constants, store to temp first then get address
                constTemp <- newTemp "const" argType
                addrTemp <- newTemp "addr" (IRPtr argType)
                let assignInstr = IRASSIGN constTemp argOp argType
                    addrInstr = IRADDR addrTemp constTemp (IRPtr argType)
                pure (argInstrs ++ [assignInstr, addrInstr], IRTemp addrTemp (IRPtr argType))
            else pure (prepareArg (argInstrs, argOp, argType))
          
          registerCall matchedName
          retTemp <- newTemp "t" irRetType
          let callInstr = IRCALL retTemp matchedName [finalOp] (Just irRetType)
          pure (finalInstrs ++ [callInstr], IRTemp retTemp irRetType)
        
        Nothing -> do
          -- Fallback: use the base name (might be extern like printf)
          let (argInstrs', argOp') = prepareArg (argInstrs, argOp, argType)
          registerCall baseName
          retTemp <- newTemp "t" defaultRetType
          let callInstr = IRCALL retTemp baseName [argOp'] (Just defaultRetType)
          pure (argInstrs' ++ [callInstr], IRTemp retTemp defaultRetType)
    
    head' :: [a] -> a
    head' (x:_) = x
    head' [] = error "head': empty list"
    
    orElse :: Maybe a -> Maybe a -> Maybe a
    orElse (Just x) _ = Just x
    orElse Nothing y = y
    
    irTypeToASTType :: IRType -> Type
    irTypeToASTType IRI8 = TypeI8
    irTypeToASTType IRI16 = TypeI16
    irTypeToASTType IRI32 = TypeI32
    irTypeToASTType IRI64 = TypeI64
    irTypeToASTType IRF32 = TypeF32
    irTypeToASTType IRF64 = TypeF64
    irTypeToASTType IRBool = TypeBool
    irTypeToASTType IRU8 = TypeU8
    irTypeToASTType IRU16 = TypeU16
    irTypeToASTType IRU32 = TypeU32
    irTypeToASTType IRU64 = TypeU64
    irTypeToASTType IRChar = TypeChar
    irTypeToASTType IRNull = TypeNull
    irTypeToASTType (IRPtr IRChar) = TypeString
    irTypeToASTType (IRPtr t) = TypePtr (irTypeToASTType t)
    irTypeToASTType (IRRef t) = TypeRef (irTypeToASTType t)
    irTypeToASTType (IRStruct s) = TypeCustom s
    irTypeToASTType (IRArray t _) = TypeArray (irTypeToASTType t)
    irTypeToASTType (IRVariadic t) = TypeVariadic (irTypeToASTType t)
    
    isTypeCompatible' :: Type -> Type -> Bool
    isTypeCompatible' TypeAny _ = True
    isTypeCompatible' _ TypeAny = True
    isTypeCompatible' (TypeRef a) b = isTypeCompatible' a b
    isTypeCompatible' a (TypeRef b) = isTypeCompatible' a b
    isTypeCompatible' (TypePtr TypeAny) _ = True
    isTypeCompatible' _ (TypePtr TypeAny) = True
    isTypeCompatible' TypeString (TypePtr TypeChar) = True
    isTypeCompatible' (TypePtr TypeChar) TypeString = True
    isTypeCompatible' a b = a == b || (isIntType a && isIntType b) || (isFloatType' a && isFloatType' b)
    
    isIntType :: Type -> Bool
    isIntType TypeI8 = True
    isIntType TypeI16 = True
    isIntType TypeI32 = True
    isIntType TypeI64 = True
    isIntType TypeU8 = True
    isIntType TypeU16 = True
    isIntType TypeU32 = True
    isIntType TypeU64 = True
    isIntType _ = False
    
    isFloatType' :: Type -> Bool
    isFloatType' TypeF32 = True
    isFloatType' TypeF64 = True
    isFloatType' _ = False
    
    accumulateResults :: IRType -> [IROperand] -> IRGen ([IRInstruction], IROperand)
    accumulateResults _ [] = throwError "No results to accumulate"
    accumulateResults _ [single] = pure ([], single)
    accumulateResults irRetType (first:rest) = go first rest []
      where
        go acc [] instrs = pure (instrs, acc)
        go acc (next:remaining) instrs = do
          resultTemp <- newTemp "t" irRetType
          let addInstr = IRADD_OP resultTemp acc next irRetType
          go (IRTemp resultTemp irRetType) remaining (instrs ++ [addInstr])

genArgWithContext :: GenExprCallback -> Expression -> Type -> IRGen ([IRInstruction], IROperand, IRType)
genArgWithContext genExpr expr expectedType = do
  (instrs, op, inferredType) <- genExpr expr
  let targetType = case expectedType of
                     TypeArray elemType -> IRPtr (IRArray (astTypeToIRType elemType) 0)
                     t -> astTypeToIRType t
  
  inferIfNeeded instrs op inferredType targetType
  where
    needsInference (IRConstInt _) _ _ = True
    needsInference (IRConstChar _) _ _ = True
    needsInference (IRConstBool _) _ _ = True
    needsInference (IRGlobal _ _) infT targT = isFloatType infT && isFloatType targT
    needsInference _ _ _ = False

    inferIfNeeded instrs op inferredType targetType
      | inferredType /= targetType && needsInference op inferredType targetType = do
          temp <- newTemp "arg" targetType
          let assign = IRASSIGN temp op targetType
          pure (instrs <> [assign], IRTemp temp targetType, targetType)
      | otherwise =
          pure (instrs, op, inferredType)

--
-- private
--

prepareArg :: ([IRInstruction], IROperand, IRType) -> ([IRInstruction], IROperand)
prepareArg (i, IRTemp n t, IRStruct _) = (i <> [IRADDR ("p_" <> n) n (IRPtr t)], IRTemp ("p_" <> n) (IRPtr t))
prepareArg (i, IRTemp n t, IRPtr (IRStruct _)) = (i <> [IRADDR ("p_" <> n) n (IRPtr t)], IRTemp ("p_" <> n) (IRPtr t))
prepareArg (i, op, _) = (i, op)
