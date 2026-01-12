module Rune.Semantics.Type
  ( VarStack
  , FuncStack
  , StructStack
  , Stack
  , Templates
  , FuncInfo
  ) where

import Data.HashMap.Strict (HashMap)
import Rune.AST.Nodes (Type, TopLevelDef)

--
-- public
--

type VarStack = HashMap String Type
-- | FuncInfo: (returnType, paramTypes, variadicType, isExternal)
-- variadicType is Nothing for non-variadic functions, Just Type for variadic
-- isExternal is True for functions declared in 'somewhere' blocks
type FuncInfo = (Type, [Type], Maybe Type, Bool)
type FuncStack = HashMap String FuncInfo
type StructStack = HashMap String TopLevelDef
type Stack = (FuncStack, VarStack, StructStack)

type Templates = HashMap String TopLevelDef
