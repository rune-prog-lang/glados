module IR.IRExpressionsSpec (irExpressionsTests) where

import Control.Monad.State (runState)
import Data.Map (empty)
import qualified Data.Set as Set
import Rune.AST.Nodes (Expression (..), Type (..))
import Rune.IR.Generator.GenExpression (genExpression)
import Rune.IR.Nodes
  ( GenState (..),
    IRGen,
    IRInstruction (..),
    IROperand (..),
    IRTopLevel (IRGlobalString),
    IRType (..),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

--
-- helpers for testing
--

initialState :: GenState
initialState =
  GenState
    { gsTempCounter = 0,
      gsLabelCounter = 0,
      gsStringCounter = 0,
      gsGlobals = [],
      gsCurrentFunc = Nothing,
      gsSymTable = empty,
      gsStructs = empty,
      gsLoopStack = [],
      gsCalledFuncs = Set.empty
    }

runIRGen :: IRGen a -> (a, GenState)
runIRGen m = runState m initialState

--
-- public
--

irExpressionsTests :: TestTree
irExpressionsTests =
  testGroup
    "Rune.IR.Literals Specs"
    [ testCase "genLitInt" testGenLitInt,
      testCase "genLitFloat" testGenLitFloat,
      testCase "genLitChar" testGenLitChar,
      testCase "genLitBool True" testGenLitBoolTrue,
      testCase "genLitBool False" testGenLitBoolFalse,
      testCase "genLitNull" testGenLitNull,
      testCase "genLitString" testGenLitString,
      testCase "genCast i32 to f32" testGenCastI32ToF32,
      testCase "genCast f32 to i32" testGenCastF32ToI32,
      testCase "genCast i32 to string" testGenCastI32ToString,
      testCase "genCast" testGenCast
    ]

--
-- private
--

testGenCast :: IO ()
testGenCast =
  let expr = ExprCast (ExprLitInt 42) TypeF32
      (result, _) = runIRGen (genExpression expr)
      expectedInstrs = [IRCAST "t0" (IRConstInt 42) IRI32 IRF32]
      expectedOperand = IRTemp "t0" IRF32
      expectedType = IRF32
   in result @?= (expectedInstrs, expectedOperand, expectedType)

testGenLitInt :: IO ()
testGenLitInt =
  let expr = ExprLitInt 42
      (result, _) = runIRGen (genExpression expr)
      expected = ([], IRConstInt 42, IRI32)
   in result @?= expected

testGenLitFloat :: IO ()
testGenLitFloat =
  let expr = ExprLitFloat 3.14
      (result, _) = runIRGen (genExpression expr)
      expected = ([], IRConstFloat 3.14, IRF32)
   in result @?= expected

testGenLitChar :: IO ()
testGenLitChar =
  let expr = ExprLitChar 'a'
      (result, _) = runIRGen (genExpression expr)
      expected = ([], IRConstChar 'a', IRChar)
   in result @?= expected

testGenLitBoolTrue :: IO ()
testGenLitBoolTrue =
  let expr = ExprLitBool True
      (result, _) = runIRGen (genExpression expr)
      expected = ([], IRConstInt 1, IRI32)
   in result @?= expected

testGenLitBoolFalse :: IO ()
testGenLitBoolFalse =
  let expr = ExprLitBool False
      (result, _) = runIRGen (genExpression expr)
      expected = ([], IRConstInt 0, IRI32)
   in result @?= expected

testGenLitNull :: IO ()
testGenLitNull =
  let expr = ExprLitNull
      (result, _) = runIRGen (genExpression expr)
      expected = ([], IRConstInt 0, IRNull)
   in result @?= expected

testGenLitString :: IO ()
testGenLitString =
  let expr = ExprLitString "hello world"
      (result, finalState) = runIRGen (genExpression expr)

      expectedInstrs = [IRADDR "p_ptr0" "str_global0" (IRPtr IRChar)]
      expectedOperand = IRTemp "p_ptr0" (IRPtr IRChar)
      expectedType = IRPtr IRChar
      expectedGlobals = [IRGlobalString "str_global0" "hello world"]
   in do
        result @?= (expectedInstrs, expectedOperand, expectedType)
        gsGlobals finalState @?= expectedGlobals
        gsStringCounter finalState @?= 1

testGenCastI32ToF32 :: IO ()
testGenCastI32ToF32 =
  let expr = ExprCast (ExprLitInt 42) TypeF32
      (result, _) = runIRGen (genExpression expr)
      expectedInstrs = [IRCAST "t0" (IRConstInt 42) IRI32 IRF32]
      expectedOperand = IRTemp "t0" IRF32
      expectedType = IRF32
   in result @?= (expectedInstrs, expectedOperand, expectedType)

testGenCastF32ToI32 :: IO ()
testGenCastF32ToI32 =
  let expr = ExprCast (ExprLitFloat 3.14) TypeI32
      (result, _) = runIRGen (genExpression expr)
      expectedInstrs = [IRCAST "t0" (IRConstFloat 3.14) IRF32 IRI32]
      expectedOperand = IRTemp "t0" IRI32
      expectedType = IRI32
   in result @?= (expectedInstrs, expectedOperand, expectedType)

testGenCastI32ToString :: IO ()
testGenCastI32ToString =
  let expr = ExprCast (ExprLitInt 42) TypeString
      (result, _) = runIRGen (genExpression expr)
      expectedInstrs = [IRCAST "t0" (IRConstInt 42) IRI32 (IRPtr IRChar)]
      expectedOperand = IRTemp "t0" (IRPtr IRChar)
      expectedType = IRPtr IRChar
   in result @?= (expectedInstrs, expectedOperand, expectedType)
