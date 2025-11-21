module PipelinesSpec (pipelinesTests) where

import Control.Exception (try)
import Control.Monad (when)
import Rune.Pipelines (compilePipeline, interpretPipeline)
import System.Directory (doesFileExist, removeFile)
import System.IO.Error (isDoesNotExistError)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

--
-- public
--

pipelinesTests :: TestTree
pipelinesTests =
  testGroup
    "Rune Pipelines Tests"
    [ compilePipelineTests,
      interpretPipelineTests
    ]

--
-- private helpers
--

expectedHelloRuneIR :: String
expectedHelloRuneIR =
  "Program: examples/hello_rune.ru\n"
    ++ "  DefFunction main\n"
    ++ "    Parameters:\n"
    ++ "    ReturnType: null\n"
    ++ "    Body:\n"
    ++ "      StmtExpr\n"
    ++ "        ExprCall show\n"
    ++ "        Arguments:\n"
    ++ "          ExprLitString \"Hello, Rune!\\n\""

readFileAndCleanup :: FilePath -> IO String
readFileAndCleanup filePath = do
  contentResult <- try (readFile filePath) :: IO (Either IOError String)
  case contentResult of
    Left ex | isDoesNotExistError ex -> return ""
    Left ex -> assertFailure $ "Failed to read output file " ++ filePath ++ ": " ++ show ex
    Right content -> do
      removeFile filePath
      return $ reverse $ dropWhile (`elem` "\n\r") $ reverse content

compilePipelineTests :: TestTree
compilePipelineTests = testCase "compilePipeline (hello_rune.ru)" $ do
  let inFile = "examples/hello_rune.ru"
  let outFile = "out.ir"
  exists <- doesFileExist outFile
  when exists $ removeFile outFile
  compilePipeline inFile outFile
  actualContent <- readFileAndCleanup outFile
  actualContent @?= expectedHelloRuneIR

interpretPipelineTests :: TestTree
interpretPipelineTests = testCase "interpretPipeline (hello_rune.ru)" $ do
  let inFile = "examples/hello_rune.ru"
  interpretPipeline inFile
  () @?= ()
