{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}

#if defined(TESTING_EXPORT)
module Rune.Pipelines
  ( CompileMode (..),
    LibraryOptions (..),
    compilePipeline,
    compileMultiplePipeline,
    compileAsmToObject,
    compileObjectIntoExecutable,
    linkObjectsIntoExecutable,
    emitAssembly,
    interpretPipeline,
    pipeline,
    verifAndGenIR,
    runPipeline,
    runPipelineAction,
    optimizeIR,
    checkSemantics,
    safeRead,
    parseLexer,
    parseAST,
  )
where
#else
module Rune.Pipelines
  ( compilePipeline,
    interpretPipeline,
    compileMultiplePipeline,
    CompileMode (..),
    LibraryOptions (..)
  )
where
#endif

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (IOException, try, bracket)
import Control.Monad ((>=>), when)

import Data.Functor ((<&>))
import Data.List (partition)
import Data.Maybe (catMaybes)

import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import System.IO (hPutStrLn, hPutStr, hClose, openTempFile, stderr)

import Logger (logError)

import Rune.AST.Nodes (Program)
import Rune.AST.Parser (parseRune)
import Rune.Backend.X86_64.Codegen (emitAssembly, emitAssemblyLib)
import Rune.IR.Generator (generateIR)
import Rune.IR.Nodes (IRProgram)
import Rune.IR.Printer (prettyPrintIR)
import Rune.IR.Optimizer (runIROptimizer)
import Rune.Lexer.Lexer (lexer)
import Rune.Lexer.Tokens (Token)
import Rune.Semantics.Vars (verifVars)
import Rune.SanityChecks (performSanityChecks)
import Rune.Semantics.Type (FuncStack)

import Lib (fixpoint)

import Text.Megaparsec (errorBundlePretty)
import System.Process (rawSystem)
import System.FilePath (takeExtension, dropExtension)
import System.Directory (removeFile, getTemporaryDirectory, getHomeDirectory)

data LibraryOptions = LibraryOptions
  { libShared    :: Bool
  , libStatic    :: Bool
  , libPaths     :: [FilePath]
  , libNames     :: [String]
  } deriving (Show, Eq)

data CompileMode
  = ToObject
  | ToExecutable LibraryOptions
  | ToAssembly
  | FullCompile LibraryOptions
  deriving (Show, Eq)

--
-- public
--

--
-- compiler pipeline routine
--

-- | rune build <file.ru> -o output [options]
compilePipeline :: FilePath -> FilePath -> CompileMode -> IO ()
compilePipeline inFile outFile mode =
  case mode of
    FullCompile libOpts -> compileFullPipeline inFile outFile libOpts
    ToAssembly          -> compileToAssembly inFile outFile
    ToObject            -> compileToObject inFile outFile
    ToExecutable libOpts-> compileObjectIntoExecutable inFile outFile libOpts

-- | rune build <file.ru> -o output
compileFullPipeline :: FilePath -> FilePath -> LibraryOptions -> IO ()
compileFullPipeline inFile outFile libOpts =
  runPipelineAction inFile $ \ir -> do
    let objFile = dropExtension inFile <> ".o"
        asmContent = if libShared libOpts || libStatic libOpts
                     then emitAssemblyLib ir
                     else emitAssembly ir
    compileAsmToObject asmContent objFile (libShared libOpts)
    linkOrArchive libOpts [objFile] outFile

-- | rune build <file.ru> -S -o output.asm
compileToAssembly :: FilePath -> FilePath -> IO ()
compileToAssembly inFile outFile =
  runPipelineAction inFile (writeFile outFile . emitAssembly)

-- | rune build <file.ru|file.asm> -o output.o
compileToObject :: FilePath -> FilePath -> IO ()
compileToObject inFile outFile =
  case takeExtension inFile of
    ".ru" -> runPipelineAction inFile $ \ir ->
              let asmContent = emitAssembly ir
               in compileAsmToObject asmContent outFile False
    ".asm" -> safeRead inFile >>= either logError (\c -> compileAsmToObject c outFile False)
    ext -> logError $ "Unsupported file extension: " <> ext

-- | rune build <file1.ru> <file2.ru> ... -o output
compileMultiplePipeline :: [FilePath] -> FilePath -> LibraryOptions -> IO ()
compileMultiplePipeline [] _ _ = logError "No input files provided."
compileMultiplePipeline inFiles outFile libOpts = do
  let (runeFiles, remainder) = partition (\fp -> takeExtension fp == ".ru") inFiles
      (asmFiles, objectFiles) = partition (\fp -> takeExtension fp == ".asm") remainder
      isLib = libShared libOpts || libStatic libOpts

  runeObjs <- compileRuneSources runeFiles outFile isLib
  asmObjs  <- compileAsmSources asmFiles isLib
  let allObjects = objectFiles <> asmObjs <> runeObjs
  case allObjects of
    [] -> logError "No object files to link."
    _  -> linkOrArchive libOpts allObjects outFile

-- | compile many Rune sources files into object files concurrently
-- rune build <file1.ru> <file2.ru> ... -o output
--
-- threads:
--  1. compile each file separately in parallel
--  2. return list of object files
--  3. log errors as they occur
compileRuneSources :: [FilePath] -> FilePath -> Bool -> IO [FilePath]
compileRuneSources [] _ _ = pure []
compileRuneSources runeFiles _ isLib = do
  performSanityChecks >>= either (\e -> logError e >> pure []) (\() -> do
    results <- mapConcurrently (compileRuneFile isLib) runeFiles
    let (errors, successes) = partitionResults results
    mapM_ (logErrorNoExit) errors
    when (not (null errors)) $ exitWith (ExitFailure 84)
    pure successes
    )
  where
    compileRuneFile :: Bool -> FilePath -> IO (Either String FilePath)
    compileRuneFile forLib runeFile = do
      result <- runPipeline runeFile
      case result of
        Left err -> pure (Left err)
        Right ir -> do
          let objFile = dropExtension runeFile <> ".o"
              asmContent = if forLib then emitAssemblyLib ir else emitAssembly ir
          compileAsmToObject asmContent objFile forLib
          pure (Right objFile)
    
    partitionResults :: [Either String FilePath] -> ([String], [FilePath])
    partitionResults = foldr go ([], [])
      where
        go (Left err) (errs, succs) = (err : errs, succs)
        go (Right fp) (errs, succs) = (errs, fp : succs)
    
    logErrorNoExit :: String -> IO ()
    logErrorNoExit msg = do
      let red = "\x1b[31m"
      let reset = "\x1b[0m"
      hPutStrLn stderr $ red ++ "[ERROR]: " ++ reset ++ msg

-- | compile a list of assembly files into object files concurrently
compileAsmSources :: [FilePath] -> Bool -> IO [FilePath]
compileAsmSources asmFiles isLib = do
  let targets = map (\fp -> (fp, dropExtension fp <> ".o")) asmFiles
  results <- mapConcurrently (compileTarget isLib) targets
  pure (catMaybes results)
  where
    compileTarget forLib (asmFile, objFile) =
      safeRead asmFile >>= either (\e -> logError e >> pure Nothing) (\asmContent -> compileAsmToObject asmContent objFile forLib >> pure (Just objFile))

interpretPipeline :: FilePath -> IO ()
interpretPipeline inFile = runPipelineAction inFile (putStr . prettyPrintIR)

--
-- private pipelines
--

pipeline :: (FilePath, String) -> Either String IRProgram
pipeline =
  parseLexer
    >=> parseAST
    >=> verifAndGenIR
    >=> optimizeIR

verifAndGenIR :: Program -> Either String IRProgram
verifAndGenIR p = do
  (checkedAST, funcStack) <- checkSemantics p
  generateIR checkedAST funcStack

runPipeline :: FilePath -> IO (Either String IRProgram)
runPipeline fp = do
  performSanityChecks >>= either (pure . Left)
    (\() -> safeRead fp <&> (>>= (pipeline . (fp,))))


runPipelineAction :: FilePath -> (IRProgram -> IO ()) -> IO ()
runPipelineAction inFile onSuccess =
  runPipeline inFile >>= either logError onSuccess

---
--- private methods for compilation steps
---

compileAsmToObject :: String -> FilePath -> Bool -> IO ()
compileAsmToObject asmContent objFile forPic = do
  tmpDir <- getTemporaryDirectory
  bracket (openTempFile tmpDir "asm-XXXXXX.asm")
          (\(asmFile, h) -> hClose h >> removeFile asmFile)
          (\(asmFile, h) -> do
              hPutStr h asmContent >> hClose h
              let nasmArgs = if forPic
                             then ["-f", "elf64", "-DPIC", asmFile, "-o", objFile]
                             else ["-f", "elf64", asmFile, "-o", objFile]
              exitCode <- rawSystem "nasm" nasmArgs
              when (exitCode /= ExitSuccess) $
                logError $ "Assembly to object compilation failed with exit code: " <> show exitCode)

compileObjectIntoExecutable :: FilePath -> FilePath -> LibraryOptions -> IO ()
compileObjectIntoExecutable objFile outFile libOpts = linkOrArchive libOpts [objFile] outFile

linkOrArchive :: LibraryOptions -> [FilePath] -> FilePath -> IO ()
linkOrArchive libOpts objFiles outFile
  | libStatic libOpts = createStaticLibrary objFiles outFile
  | libShared libOpts = createSharedLibrary objFiles outFile libOpts
  | otherwise         = linkObjectsIntoExecutable objFiles outFile libOpts

createStaticLibrary :: [FilePath] -> FilePath -> IO ()
createStaticLibrary objFiles outFile = do
  exitCode <- rawSystem "ar" (["rcs", outFile] ++ objFiles)
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure code -> logError $ "Static library creation failed with exit code: " <> show code

createSharedLibrary :: [FilePath] -> FilePath -> LibraryOptions -> IO ()
createSharedLibrary objFiles outFile libOpts = do
  home <- getHomeDirectory
  let defaultLibPaths = ["/usr/local/lib", "/usr/lib", home <> "/.local/lib", "./lib/std"]
      libPathArgs = concatMap (\p -> ["-L" <> p]) (defaultLibPaths ++ libPaths libOpts)
      libNameArgs = concatMap (\n -> ["-l" <> n]) (libNames libOpts)
      rpathArgs   = ["-Wl,-rpath," <> home <> "/.local/lib"]
      args = ["-shared", "-fPIC", "-o", outFile] ++ objFiles ++ libPathArgs ++ libNameArgs ++ rpathArgs
  exitCode <- rawSystem "gcc" args
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure code -> logError $ "Shared library creation failed with exit code: " <> show code

linkObjectsIntoExecutable :: [FilePath] -> FilePath -> LibraryOptions -> IO ()
linkObjectsIntoExecutable objFiles exeFile libOpts = do
  home <- getHomeDirectory
  let defaultLibPaths = ["/usr/local/lib", "/usr/lib", home <> "/.local/lib", "./lib/std"]
      libPathArgs = concatMap (\p -> ["-L" <> p]) (defaultLibPaths ++ libPaths libOpts)
      libNameArgs = concatMap (\n -> ["-l" <> n]) (libNames libOpts)
      rpathArgs   = ["-Wl,-rpath," <> home <> "/.local/lib"]
      args = ["-no-pie"] ++ objFiles ++ ["-o", exeFile] ++ libPathArgs ++ libNameArgs ++ rpathArgs
  exitCode <- rawSystem "gcc" args
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure code -> logError $ "Object to executable compilation failed with exit code: " <> show code

--
-- private encapsulations for error handling
--

optimizeIR :: IRProgram -> Either String IRProgram
optimizeIR = Right . fixpoint runIROptimizer

checkSemantics :: Program -> Either String (Program, FuncStack)
checkSemantics = verifVars

safeRead :: FilePath -> IO (Either String String)
safeRead fp = do
  r <- try (readFile fp) :: IO (Either IOException String)
  pure $ either (Left . ("Failed to read input file: " <>) . show) Right r

parseLexer :: (FilePath, String) -> Either String (FilePath, [Token])
parseLexer (fp, content) = either (Left . errorBundlePretty) (Right . (fp,)) (lexer fp content)

parseAST :: (FilePath, [Token]) -> Either String Program
parseAST (fp, tokens) = Right =<< parseRune fp tokens
