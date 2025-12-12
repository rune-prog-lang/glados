{-# LANGUAGE CPP #-}
#define TESTING_EXPORT

module IR.Generator.Expression.CastSpecs (castExprTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Rune.IR.Generator.Expression.Cast
import Rune.IR.Nodes (IRType(..), IROperand(..), IRInstruction(..))
import Rune.AST.Nodes (Expression(..), Type(..))
import IR.TestUtils (runGen)

--
-- public
--

castExprTests :: TestTree
castExprTests = testGroup "Rune.IR.Generator.Expression.Cast"
  [ testGenCast
  ]

--
-- private
--

testGenCast :: TestTree
testGenCast = testGroup "genCast"
  [ testCase "Generates IRCAST instruction" $
      let genExpr (ExprLitInt n) = return ([], IRConstInt n, IRI32)
          genExpr _ = return ([], IRConstInt 0, IRI32)
          (instrs, _, typ) = runGen (genCast genExpr (ExprLitInt 42) TypeF64)
      in do
        assertBool "Should have IRCAST instruction" $ not $ null instrs
        case last instrs of
          IRCAST _ (IRConstInt 42) IRF64 -> return ()
          _ -> assertBool "Expected IRCAST" False
        typ @?= IRF64

  , testCase "Optimization: identity cast returns operand" $
      let genExpr (ExprLitInt n) = return ([], IRConstInt n, IRI32)
          genExpr _ = return ([], IRConstInt 0, IRI32)
          (instrs, op, typ) = runGen (genCast genExpr (ExprLitInt 42) TypeI32)
      in do
        instrs @?= []
        op @?= IRConstInt 42
        typ @?= IRI32
  ]
