{-# LANGUAGE LambdaCase #-}

module Rune.Backend.X86_64.Codegen
  ( emitAssembly,
  )
where

import Data.List (nub)
import Lib (escapeString)
import Rune.Backend.Types (Extern, Function, GlobalString)
import Rune.IR.Nodes

--
-- public
--

--
-- private helpers
--

emit :: Int -> String -> String
emit lvl s = replicate (lvl * 4) ' ' ++ s

emitAssembly :: IRProgram -> String
emitAssembly (IRProgram _ topLevels) =
  let (externs, globalStrings, functions) = collectTopLevel topLevels
   in unlines $
        emitExterns externs
          ++ emitDataSection globalStrings
          ++ emitTextSection functions

--
-- top level
--

-- | collext externs, global strings, and functions
collectTopLevel :: [IRTopLevel] -> ([Extern], [GlobalString], [Function])
collectTopLevel tls =
  let (es, gs, fs) = foldr go ([], [], []) tls
   in (nub es, reverse gs, reverse fs)
  where
    go (IRExtern name) (e, g, f) = (name : e, g, f)
    go (IRGlobalString n v) (e, g, f) = (e, (n, v) : g, f)
    go (IRFunctionDef fn) (e, g, f) = (e, g, fn : f)
    go _ acc = acc

-- | extern <function>
emitExterns :: [Extern] -> [String]
emitExterns [] = []
emitExterns xs = map (\n -> "extern " ++ n) xs

--
-- section .data
--

-- | emit global strings
-- <name> db "<value>", 0
emitDataSection :: [GlobalString] -> [String]
emitDataSection [] = []
emitDataSection gs = "section .data" : map emitGlobal gs
  where
    emitGlobal (name, val) = name ++ " db \"" ++ escapeString val ++ "\", 0"

--
-- section .text
--

emitTextSection :: [Function] -> [String]
emitTextSection [] = []
emitTextSection fs = "section .text" : concatMap emitFunction fs

--
-- function emission
--

-- | emit function
-- global <name>
-- <name>:
--     push rbp
--     mov rbp, rsp
--     ...
--     pop rbp
--     ret
emitFunction :: Function -> [String]
emitFunction (IRFunction name _ _ _) =
  [ "global " ++ name,
    name ++ ":",
    emit 1 "push rbp",
    emit 1 "mov rbp, rsp",
    emit 1 "; ... function body ...",
    emit 1 "pop rbp",
    emit 1 "ret",
    ""
  ]
