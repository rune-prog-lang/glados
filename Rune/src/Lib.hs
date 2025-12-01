module Lib (escapeString) where

--
-- public
--

escapeString :: String -> String
escapeString = concatMap escapeChar
  where
    escapeChar '\0' = "\\0"
    escapeChar '\n' = "\\n"
    escapeChar '\t' = "\\t"
    escapeChar '\r' = "\\r"
    escapeChar '\\' = "\\\\"
    escapeChar '"' = "\\\""
    escapeChar c = [c]
