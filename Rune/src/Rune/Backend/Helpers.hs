module Rune.Backend.Helpers
  ( emit,
    escapeString,
    collectIRVars,
    collectTopLevels,
  )
where

import Data.List (intercalate, nub)
import Data.Map.Strict (Map, fromList, insert)
import Rune.Backend.Types (Extern, Function, GlobalString)
import Rune.IR.Nodes (IRFunction (..), IRInstruction (..), IRTopLevel (..), IRType (..))

--
-- public
--

emit :: Int -> String -> String
emit lvl s = replicate (lvl * 4) ' ' ++ s

escapeString :: String -> String
escapeString s = intercalate "," (map escapeChar s)

collectTopLevels :: [IRTopLevel] -> ([Extern], [GlobalString], [Function])
collectTopLevels tls =
  let (es, gs, fs) = foldr collectTopLevel ([], [], []) tls
   in (nub es, reverse gs, reverse fs)

collectIRVars :: Function -> Map String IRType
collectIRVars (IRFunction _ params _ body) =
  let initialMap = fromList (map (\(n, t) -> (n, t)) params)
   in foldl' collectVars initialMap body

--
-- private
--

escapeChar :: Char -> String
escapeChar '\0' = "0"
escapeChar '\n' = "10"
escapeChar '\t' = "9"
escapeChar '\r' = "13"
escapeChar '\\' = "'\\\\'"
escapeChar '"' = "'\"'"
escapeChar '\'' = "'''"
escapeChar c
  | c >= ' ' && c <= '~' = "'" ++ [c] ++ "'"
  | otherwise = show (fromEnum c)

collectTopLevel :: IRTopLevel -> ([Extern], [GlobalString], [Function]) -> ([Extern], [GlobalString], [Function])
collectTopLevel (IRExtern name) (e, g, f) = (name : e, g, f)
collectTopLevel (IRGlobalString n v) (e, g, f) = (e, (n, v) : g, f)
collectTopLevel (IRFunctionDef fn) (e, g, f) = (e, g, fn : f)
collectTopLevel _ acc = acc

collectVars :: Map String IRType -> IRInstruction -> Map String IRType
collectVars acc (IRASSIGN n _ t) = insert n t acc
collectVars acc (IRALLOC n t) = insert n t acc
collectVars acc (IRLOAD n _ t) = insert n t acc
collectVars acc (IRDEREF n _ t) = insert n t acc
collectVars acc (IRGET_FIELD n _ _ _ t) = insert n t acc
collectVars acc (IRADD_OP n _ _ t) = insert n t acc
collectVars acc (IRSUB_OP n _ _ t) = insert n t acc
collectVars acc (IRMUL_OP n _ _ t) = insert n t acc
collectVars acc (IRDIV_OP n _ _ t) = insert n t acc
collectVars acc (IRMOD_OP n _ _ t) = insert n t acc
collectVars acc (IRCMP_EQ n _ _) = insert n IRI32 acc
collectVars acc (IRCMP_NEQ n _ _) = insert n IRI32 acc
collectVars acc (IRCMP_LT n _ _) = insert n IRI32 acc
collectVars acc (IRCMP_LTE n _ _) = insert n IRI32 acc
collectVars acc (IRAND_OP n _ _ t) = insert n t acc
collectVars acc (IROR_OP n _ _ t) = insert n t acc
collectVars acc (IRCALL n _ _ (Just t)) = insert n t acc
collectVars acc (IRADDR n _ t) = insert n t acc
collectVars acc _ = acc
