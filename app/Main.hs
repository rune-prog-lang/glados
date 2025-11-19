module Main (main) where

import CLI (parseArgs, runCLI)
import Logger (logError)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Left err -> logError err
        Right action -> runCLI action
