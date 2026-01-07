module Rune.Semantics.Struct (findStruct) where

import Text.Printf (printf)
import Control.Monad (foldM, when)

import Rune.AST.Nodes
import Rune.Semantics.Type (StructStack)

import qualified Data.HashMap.Strict as HM
import qualified Data.List as List

---
--- public
---

findStruct :: Program -> Either String StructStack
findStruct (Program _ defs) = do
  let structs = [d | d@DefStruct {} <- defs]
  foldM addStruct HM.empty structs
  where
    addStruct acc (DefStruct name fields methods) = do
      when (HM.member name acc) $
        Left $ printf "HasDuplicates: Struct '%s' is already defined" name
      checkedFields <- checkFields name acc fields
      checkedMethods <- checkMethods name methods
      Right $ HM.insert name (DefStruct name checkedFields checkedMethods) acc
    addStruct acc _ = Right acc

---
--- private
---

checkMethods :: String -> [TopLevelDef] -> Either String [TopLevelDef]
checkMethods sName methods = do
  let defFuncNames = [n | DefFunction n _ _ _ _ <- methods]
      funcDuplicates = defFuncNames List.\\ List.nub defFuncNames
  case funcDuplicates of
    (dup:_) -> Left $ printf "Duplicate method '%s' in struct '%s' (use override for additional signatures)" dup sName
    [] -> Right methods

checkFields :: String -> StructStack -> [Field] -> Either String [Field]
checkFields sName structs fields = do
  let fieldNames = map fieldName fields
      duplicates = fieldNames List.\\ List.nub fieldNames
  case duplicates of
    (dup:_) -> Left $ printf "Duplicate field '%s' in struct '%s'" dup sName
    [] -> mapM (validateFieldType sName structs) fields

validateFieldType :: String -> StructStack -> Field -> Either String Field
validateFieldType sName structs field = do
  case fieldType field of
    TypeAny -> Left $
      printf "Field '%s' in struct '%s' cannot have type 'any'" (fieldName field) sName
    TypeNull -> Left $
      printf "Field '%s' in struct '%s' cannot have type 'null'" (fieldName field) sName
    TypeCustom customType ->
      case customType == sName || HM.member customType structs of
        True -> Right field
        False -> Left $ printf "Field '%s' in struct '%s' references unknown type '%s'" (fieldName field) sName customType
    _ -> Right field
