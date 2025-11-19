module CLI
  ( Action (..),
    runCLI,
    parseArgs,
  )
where

import System.Exit (exitSuccess)

data Action
  = ShowUsage
  | Compile FilePath (Maybe FilePath)
  | Interpret FilePath
  deriving (Show, Eq)

-- |
-- - get Usage information for the CLI
usage :: String
usage =
  unlines
    [ "Usage: glados <command> [file] [options]",
      "",
      "Commands:",
      "  help           Show this help message",
      "  build [file]   Compile the given source file",
      "  run   [file]   Interpret the given source file",
      "",
      "Options:",
      "  -o, --output   Specify the output file for compilation"
    ]

-- |
-- - Parse command line arguments into an Action
-- -
-- - Returns either an error message or the corresponding Action
parseArgs :: [String] -> Either String Action
parseArgs xs = case xs of
  [] -> Left "No command provided. Use 'glados help'."
  (cmd : rest) -> parseCommand cmd rest

-- |
-- - Execute the given Action
runCLI :: Action -> IO ()
runCLI ShowUsage = putStrLn usage >>= const exitSuccess
runCLI (Interpret inFile) = putStrLn ("Interpreting file: " ++ inFile)
runCLI (Compile inFile maybeOutFile) =
  case maybeOutFile of
    Nothing ->
      putStrLn ("Compiling file '" ++ inFile ++ "' to default output...")
    Just outFile ->
      putStrLn ("Compiling file '" ++ inFile ++ "' to output file: " ++ outFile)

-- |
-- - Helper function to parse commands and their arguments
parseCommand :: String -> [String] -> Either String Action
parseCommand cmd rest = case cmd of
  "help" -> pure ShowUsage
  "--help" -> pure ShowUsage
  "-h" -> pure ShowUsage
  "run" -> parseRun rest
  "--run" -> parseRun rest
  "-r" -> parseRun rest
  "build" -> parseBuild rest
  "--build" -> parseBuild rest
  "-b" -> parseBuild rest
  _ -> Left $ "Invalid command: " ++ cmd ++ ". Use 'glados help'."

-- |
-- - Helper function to parse 'run' command arguments
parseRun :: [String] -> Either String Action
parseRun [file] = Right (Interpret file)
parseRun [] = Left "The 'run' command requires an input file."
parseRun _ = Left "The 'run' command takes exactly one file argument."

-- |
-- - Helper function to parse 'build' command arguments
parseBuild :: [String] -> Either String Action
parseBuild [] = Left "The 'build' command requires an input file."
parseBuild [file] = Right (Compile file Nothing)
parseBuild [file, "-o", outFile] = Right (Compile file (Just outFile))
parseBuild [file, "--output", outFile] = Right (Compile file (Just outFile))
parseBuild xs = Left $ "Invalid arguments for build command: " ++ unwords xs
