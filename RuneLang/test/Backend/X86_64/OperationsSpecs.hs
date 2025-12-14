{-# LANGUAGE CPP #-}
#define TESTING_EXPORT

module Backend.X86_64.OperationsSpecs (operationsTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)
import Rune.Backend.X86_64.Operations
import Rune.IR.Nodes (IRType(..), IROperand(..))
import qualified Data.Map.Strict as Map

--
-- public
--

operationsTests :: TestTree
operationsTests = testGroup "Rune.Backend.X86_64.Operations"
  [ testEmitBinaryOp
  , testEmitDivOp
  , testEmitFloatBinaryOp
  , testEmitFloatDivOp
  , testEmitIntDivOp
  , testEmitSmallMul
  ]

--
-- private
--

testEmitBinaryOp :: TestTree
testEmitBinaryOp = testGroup "emitBinaryOp"
  [ testCase "Emits integer add" $
      let sm = Map.singleton "dest" (-4)
          instrs = emitBinaryOp sm "dest" "add" (IRConstInt 1) (IRConstInt 2) IRI32
      in do
        assertBool "Contains add instruction" $ any ("add eax, ebx" ==) (map (drop 4) instrs)
        assertBool "Stores result" $ any ("mov dword [rbp-4], eax" ==) (map (drop 4) instrs)

  , testCase "Redirects to float op" $
      let sm = Map.singleton "dest" (-4)
          instrs = emitBinaryOp sm "dest" "add" (IRConstFloat 1.0) (IRConstFloat 2.0) IRF32
      in assertBool "Uses addss" $ any ("addss" `elem`) (map words instrs)

  , testCase "Redirects to small mul" $
      let sm = Map.singleton "dest" (-2)
          instrs = emitBinaryOp sm "dest" "imul" (IRConstInt 1) (IRConstInt 2) IRI16
      in assertBool "Uses imul eax, edx" $ elem "imul eax, edx" (map (drop 4) instrs)
  ]

testEmitDivOp :: TestTree
testEmitDivOp = testGroup "emitDivOp"
  [ testCase "Emits integer div" $
      let sm = Map.singleton "dest" (-4)
          instrs = emitDivOp sm "dest" (IRConstInt 10) (IRConstInt 2) IRI32
      in assertBool "Uses idiv" $ elem "idiv ebx" (map (drop 4) instrs)

  , testCase "Redirects to float div" $
      let sm = Map.singleton "dest" (-4)
          instrs = emitDivOp sm "dest" (IRConstFloat 10.0) (IRConstFloat 2.0) IRF32
      in assertBool "Uses divss" $ any ("divss" `elem`) (map words instrs)
  ]

testEmitFloatBinaryOp :: TestTree
testEmitFloatBinaryOp = testGroup "emitFloatBinaryOp"
  [ testCase "Emits addss for IRF32" $
      let sm = Map.singleton "dest" (-4)
          instrs = emitFloatBinaryOp sm "dest" "add" (IRTemp "a" IRF32) (IRTemp "b" IRF32) IRF32
      in assertBool "Uses addss" $ elem "addss xmm0, xmm1" (map (drop 4) instrs)

  , testCase "Emits subsd for IRF64" $
      let sm = Map.singleton "dest" (-8)
          instrs = emitFloatBinaryOp sm "dest" "sub" (IRTemp "a" IRF64) (IRTemp "b" IRF64) IRF64
      in assertBool "Uses subsd" $ elem "subsd xmm0, xmm1" (map (drop 4) instrs)
  ]

testEmitFloatDivOp :: TestTree
testEmitFloatDivOp = testGroup "emitFloatDivOp"
  [ testCase "Emits divss for IRF32" $
      let sm = Map.singleton "dest" (-4)
          instrs = emitFloatDivOp sm "dest" (IRTemp "a" IRF32) (IRTemp "b" IRF32) IRF32
      in assertBool "Uses divss" $ elem "divss xmm0, xmm1" (map (drop 4) instrs)
  ]

testEmitIntDivOp :: TestTree
testEmitIntDivOp = testGroup "emitIntDivOp"
  [ testCase "Emits small div for i8" $
      let sm = Map.singleton "dest" (-1)
          instrs = emitIntDivOp sm "dest" (IRConstInt 10) (IRConstInt 2) IRI8
      in assertBool "Uses idiv ecx" $ elem "idiv ecx" (map (drop 4) instrs)

  , testCase "Emits 32-bit div" $
      let sm = Map.singleton "dest" (-4)
          instrs = emitIntDivOp sm "dest" (IRConstInt 10) (IRConstInt 2) IRI32
      in assertBool "Uses cdq" $ elem "cdq" (map (drop 4) instrs)
  ]

testEmitSmallMul :: TestTree
testEmitSmallMul = testGroup "emitSmallMul"
  [ testCase "Emits mul for i16" $
      let sm = Map.singleton "dest" (-2)
          instrs = emitSmallMul sm "dest" (IRConstInt 10) (IRConstInt 2) IRI16
      in assertBool "Uses imul eax, edx" $ elem "imul eax, edx" (map (drop 4) instrs)
  ]