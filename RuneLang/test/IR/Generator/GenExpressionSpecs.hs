{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

#define TESTING_EXPORT

module IR.Generator.GenExpressionSpecs (genExpressionTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Control.Monad.State (evalState)
import Control.Monad.Except (runExceptT)
import Rune.IR.Generator.GenExpression
import Rune.IR.Nodes (IRType(..), IROperand(..), GenState(..))
import Rune.AST.Nodes (Expression(..))
import IR.TestUtils (emptyState, runGenUnsafe, runGen)
import qualified Data.Map.Strict as Map

--
-- public
--

genExpressionTests :: TestTree
genExpressionTests = testGroup "Rune.IR.Generator.GenExpression"
  [ testGenExpression
  , testGenVar
  ]

--
-- private
--

testGenExpression :: TestTree
testGenExpression = testGroup "genExpression"
  [ testCase "Generates int literal" $
      let (instrs, op, typ) = runGenUnsafe (genExpression (ExprLitInt 42))
      in do
        instrs @?= []
        op @?= IRConstInt 42
        typ @?= IRI32

  , testCase "Generates float literal" $
      let (instrs, op, typ) = runGenUnsafe (genExpression (ExprLitFloat 3.14))
      in do
        instrs @?= []
        op @?= IRGlobal "f32_global0" IRF32
        typ @?= IRF32

  , testCase "Generates char literal" $
      let (instrs, op, typ) = runGenUnsafe (genExpression (ExprLitChar 'a'))
      in do
        instrs @?= []
        op @?= IRConstChar 'a'
        typ @?= IRChar

  , testCase "Generates bool literal" $
      let (instrs, op, typ) = runGenUnsafe (genExpression (ExprLitBool True))
      in do
        instrs @?= []
        op @?= IRConstBool True
        typ @?= IRBool

  , testCase "Generates null literal" $
      let (instrs, op, typ) = runGenUnsafe (genExpression ExprLitNull)
      in do
        instrs @?= []
        op @?= IRConstNull
        typ @?= IRNull

  , testCase "Generates string literal" $
      let (_, op, typ) = runGenUnsafe (genExpression (ExprLitString "hello"))
      in do
        case op of
          IRGlobal _ (IRPtr IRChar) -> return ()
          _ -> assertBool "Expected IRGlobal" False
        typ @?= IRPtr IRChar
  ]

testGenVar :: TestTree
testGenVar = testGroup "genVar"
  [ testCase "Looks up variable in symbol table" $
      let state = emptyState { gsSymTable = Map.singleton "x" (IRTemp "x" IRI32, IRI32) }
      in case evalState (runExceptT (genVar "x")) state of
        Left err -> assertBool ("Unexpected error: " ++ err) False
        Right (instrs, op, typ) -> do
          instrs @?= []
          op @?= IRTemp "x" IRI32
          typ @?= IRI32

  , testCase "Returns Left for undefined variable" $
      case runGen (genVar "undefined") of
        Left _ -> return ()
        Right _ -> assertBool "Expected Left for undefined variable" False
  ]
