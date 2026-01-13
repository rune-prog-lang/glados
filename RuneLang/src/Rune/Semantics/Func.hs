{-# LANGUAGE CPP #-}

#if defined(TESTING_EXPORT)
module Rune.Semantics.Func
  ( findFunc
  , findDefs
  , transformStructMethods
  , mangleFuncName
  )
where
#else
module Rune.Semantics.Func (findFunc) where
#endif

import Control.Monad (foldM)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List

import Text.Printf (printf)

import Rune.AST.Nodes
import Rune.Semantics.Type (FuncStack)
import Rune.Semantics.Helper (fixSelfType)

--
-- public
--

findFunc :: Program -> Either String FuncStack
findFunc (Program _ defs) = do
  let builtins = HM.fromList
        [ ("show" , ((TypeNull, [TypeAny]), Public))
        , ("error", ((TypeNull, [TypeAny]), Public))
        ]
  foldM findDefs builtins defs

--
-- private
--

findDefs :: FuncStack -> TopLevelDef -> Either String FuncStack

-- | find function definitions
findDefs s (DefFunction name params rType _ _ _) =
    let paramTypes = map paramType params
        sig = ((rType, paramTypes), Public)
    in case HM.lookup name s of
         Nothing -> Right $ HM.insert name sig s
         Just ((existingRet, existingArgs), _) ->
             if existingRet == rType && existingArgs == paramTypes
             then Left $ printf "FuncAlreadyExist: %s was already defined with same signature" name
             else
                 let mangledName = mangleFuncName name rType paramTypes
                 in if HM.member mangledName s
                    then Left $ printf "FuncAlreadyExist: %s (mangled: %s) was already defined" name mangledName
                    else Right $ HM.insert mangledName sig s

-- | find function signatures defined somewhere else
findDefs s (DefSomewhere sigs) = foldM addSig s sigs
  where
    addSig fs (FunctionSignature name paramTypes rType) =
      let sig = ((rType, paramTypes), Public)
      in Right $ HM.insertWith (\_ old -> old) name sig fs

-- | find struct method definitions
findDefs s (DefStruct name _ methods) =
    foldM findDefs s (transformStructMethods name methods)

-- | check if a method is static (doesn't need self)
-- TODO: add maybe more static method such as static keyword idk
isStaticMethod :: String -> Bool
isStaticMethod "new" = True
isStaticMethod _     = False

transformStructMethods :: String -> [TopLevelDef] -> [TopLevelDef]
transformStructMethods sName = map transform
  where
    transform (DefFunction methodName params rType body isExport visibility) =
      let baseName = sName ++ "_" ++ methodName
          params' = if isStaticMethod methodName then params else fixSelfType sName params
      in DefFunction baseName params' rType body isExport visibility
    transform other = other

mangleFuncName :: String -> Type -> [Type] -> String
mangleFuncName fname ret args
  | TypeAny `elem` args || ret == TypeAny = fname
  | otherwise = List.intercalate "_" (show ret : fname : map show args)