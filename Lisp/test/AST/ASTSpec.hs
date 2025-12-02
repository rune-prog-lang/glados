{-# LANGUAGE OverloadedStrings #-}

module AST.ASTSpec (astTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Lisp.AST.AST

astTests :: TestTree
astTests = testGroup "AST Tests"
  [ compEnvTests
  , extractIntegerTests
  , operatorTests
  , conditionalTests
  , definitionTests
  , lambdaTests
  , sequenceTests
  , errorHandlingTests
  , listTests
  , symbolTests
  ]

compEnvTests :: TestTree
compEnvTests = testGroup "compEnv"
  [ testCase "finds variable" $ compEnv [("x", AstInteger 5)] "x" @?= Just (AstInteger 5)
  , testCase "returns Nothing when missing" $ compEnv [("x", AstInteger 5)] "z" @?= Nothing
  , testCase "handles empty environment" $ compEnv [] "x" @?= Nothing
  ]

extractIntegerTests :: TestTree
extractIntegerTests = testGroup "extractInteger"
  [ testCase "extracts from AstInteger" $ extractInteger [] (AstInteger 42) @?= Just 42
  , testCase "extracts from environment variable" $ extractInteger [("x", AstInteger 5)] (AstSymbol "x") @?= Just 5
  , testCase "returns Nothing for non-integer" $ extractInteger [] (AstBoolean True) @?= Nothing
  ]

operatorTests :: TestTree
operatorTests = testGroup "Operators"
  [ testCase "+ evaluates correctly" $ evalOp "+" 3 4 @?= Right 7
  , testCase "- evaluates correctly" $ evalOp "-" 10 3 @?= Right 7
  , testCase "* evaluates correctly" $ evalOp "*" 3 4 @?= Right 12
  , testCase "div evaluates correctly" $ evalOp "div" 10 2 @?= Right 5
  , testCase "mod evaluates correctly" $ evalOp "mod" 10 3 @?= Right 1
  , testCase "eq? returns true" $ evalOp "eq?" 5 5 @?= Right 1
  , testCase "eq? returns false" $ evalOp "eq?" 5 3 @?= Right 0
  , testCase "< returns true" $ evalOp "<" 3 5 @?= Right 1
  , testCase "< returns false" $ evalOp "<" 5 3 @?= Right 0
  , testCase "div by zero fails" $ evalOp "div" 10 0 @?= Left "Division by zero."
  , testCase "mod by zero fails" $ evalOp "mod" 10 0 @?= Left "Division by zero."
  , testCase "unknown operator fails" $ evalOp "unknown" 1 2 @?= Left "Unknown operator: unknown"
  ]

conditionalTests :: TestTree
conditionalTests = testGroup "If expressions"
  [ testCase "true branch evaluates" $ 
      evalAST [] (If (AstBoolean True) (AstInteger 1) (AstInteger 0)) @?= 
        ([], Right (AstInteger 1))
  , testCase "false branch evaluates" $ 
      evalAST [] (If (AstBoolean False) (AstInteger 1) (AstInteger 0)) @?= 
        ([], Right (AstInteger 0))
  , testCase "rejects non-boolean condition" $ 
      case evalAST [] (If (AstInteger 5) (AstInteger 1) (AstInteger 0)) of
        (_, Left _) -> return ()
        _ -> fail "Should reject non-boolean"
  , testCase "evaluates nested condition" $ do
      let cond = Call "<" [AstInteger 3, AstInteger 5]
      case evalAST [] (If cond (AstInteger 1) (AstInteger 0)) of
        (_, Right (AstInteger 1)) -> return ()
        _ -> fail "Should evaluate nested"
  ]

definitionTests :: TestTree
definitionTests = testGroup "Define"
  [ testCase "defines variable" $ do
      let (env, result) = evalAST [] (Define "x" (AstInteger 5))
      result @?= Right (AstSymbol "")
      compEnv env "x" @?= Just (AstInteger 5)
  , testCase "redefines variable" $ do
      let (env, _) = evalAST [("x", AstInteger 5)] (Define "x" (AstInteger 10))
      compEnv env "x" @?= Just (AstInteger 10)
  , testCase "rejects define with error" $ do
      case evalAST [] (Define "x" (Call "+" [AstInteger 1])) of
        (_, Left _) -> return ()
        _ -> fail "Should reject"
  , testCase "defines with evaluated expression" $ do
      let (env, _) = evalAST [("y", AstInteger 5)] (Define "x" (AstSymbol "y"))
      compEnv env "x" @?= Just (AstInteger 5)
  ]

lambdaTests :: TestTree
lambdaTests = testGroup "Lambda"
  [ testCase "captures environment" $ do
      let env = [("y", AstInteger 10)]
      case evalAST env (Lambda ["x"] (AstSymbol "y") []) of
        (_, Right (Lambda _ _ closure)) -> length closure @?= 1
        _ -> fail "Should capture"
  , testCase "calls with arguments" $ do
      let lambda = Lambda ["x"] (Call "+" [AstSymbol "x", AstInteger 10]) []
      case evalAST [("f", lambda)] (AstList [AstSymbol "f", AstInteger 5]) of
        (_, Right (AstInteger n)) -> n @?= 15
        _ -> fail "Should call"
  , testCase "rejects wrong argument count" $ do
      let lambda = Lambda ["x", "y"] (AstInteger 1) []
      case evalAST [("f", lambda)] (AstList [AstSymbol "f", AstInteger 5]) of
        (_, Left _) -> return ()
        _ -> fail "Should reject"
  ]

sequenceTests :: TestTree
sequenceTests = testGroup "Sequences"
  [ testCase "evaluates single expression" $ do
      case evalASTWithEnv [] [AstInteger 5] of
        (_, Right (AstInteger 5)) -> return ()
        _ -> fail "Should evaluate"
  , testCase "chains expressions" $ do
      case evalASTWithEnv [] [Define "x" (AstInteger 5), AstSymbol "x"] of
        (_, Right (AstInteger 5)) -> return ()
        _ -> fail "Should chain"
  , testCase "stops on error" $ do
      case evalASTWithEnv [] [AstInteger 1, AstSymbol "undefined"] of
        (_, Left _) -> return ()
        _ -> fail "Should stop"
  , testCase "propagates environment" $ do
      case evalASTWithEnv [] [Define "x" (AstInteger 5), Define "y" (AstInteger 10), AstSymbol "y"] of
        (env, Right (AstInteger 10)) -> length env @?= 2
        _ -> fail "Should propagate"
  ]

symbolTests :: TestTree
symbolTests = testGroup "Symbols"
  [ testCase "evaluates #t" $ evalAST [] (AstSymbol "#t") @?= ([], Right (AstBoolean True))
  , testCase "evaluates #f" $ evalAST [] (AstSymbol "#f") @?= ([], Right (AstBoolean False))
  , testCase "looks up variable" $ do
      let env = [("x", AstInteger 10)]
      evalAST env (AstSymbol "x") @?= (env, Right (AstInteger 10))
  , testCase "rejects undefined" $ do
      case evalAST [] (AstSymbol "undefined") of
        (_, Left _) -> return ()
        _ -> fail "Should reject"
  ]

listTests :: TestTree
listTests = testGroup "Lists"
  [ testCase "evaluates empty list" $ 
      evalAST [] (AstList []) @?= ([], Right (AstList []))
  , testCase "unwraps single element" $ 
      evalAST [] (AstList [AstInteger 5]) @?= ([], Right (AstInteger 5))
  , testCase "rejects multiple elements" $ do
      case evalAST [] (AstList [AstInteger 1, AstInteger 2]) of
        (_, Left _) -> return ()
        _ -> fail "Should reject"
  , testCase "calls operator in list" $ do
      case evalAST [] (AstList [AstSymbol "+", AstInteger 2, AstInteger 3]) of
        (_, Right (AstInteger 5)) -> return ()
        _ -> fail "Should call operator"
  ]

errorHandlingTests :: TestTree
errorHandlingTests = testGroup "Error handling"
  [ testCase "undefined variable" $ do
      case evalAST [] (AstSymbol "undefined") of
        (_, Left _) -> return ()
        _ -> fail "Should reject"
  , testCase "empty list evaluation" $ do
      case evalASTWithEnv [] [] of
        (_, Left _) -> return ()
        _ -> fail "Should reject"
  , testCase "invalid list" $ do
      case evalAST [] (AstList [AstInteger 1, AstInteger 2]) of
        (_, Left _) -> return ()
        _ -> fail "Should reject"
  , testCase "non-integer argument" $ do
      case evalAST [] (Call "+" [AstBoolean True, AstInteger 2]) of
        (_, Left _) -> return ()
        _ -> fail "Should reject"
  ]

evalOp :: String -> Int -> Int -> Either String Int
evalOp op a b = case evalAST [] (Call op [AstInteger a, AstInteger b]) of
  (_, Right (AstInteger n)) -> Right n
  (_, Right (AstBoolean True)) -> Right 1
  (_, Right (AstBoolean False)) -> Right 0
  (_, Left err) -> Left err
  _ -> Left "Unknown error"
