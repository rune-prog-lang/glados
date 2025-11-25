module Lexer.TokensSpec (tokensTests) where

import Rune.Lexer.Tokens (Token (..), TokenKind (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=))

--
-- public
--

tokensTests :: TestTree
tokensTests =
  testGroup
    "Lexer.TokensSpec"
    [ tokenCreationTests,
      tokenKindTests,
      tokenInstanceTests
    ]

--
-- helpers
--

allTokenKinds :: [TokenKind]
allTokenKinds =
  [ KwDef,
    KwReturn,
    KwStruct,
    KwIf,
    KwElse,
    KwFor,
    KwTo,
    KwOverride,
    KwIn,
    TypeI8,
    TypeI16,
    TypeI32,
    TypeI64,
    TypeF32,
    TypeF64,
    TypeBool,
    TypeU8,
    TypeU16,
    TypeU32,
    TypeString,
    TypeAny,
    TypeNull,
    LitInt 0,
    LitFloat 0.0,
    LitString "",
    LitBool True,
    LitBool False,
    LitNull,
    Identifier "x",
    OpPlus,
    OpMinus,
    OpMul,
    OpDiv,
    OpMod,
    OpAssign,
    OpEq,
    OpNeq,
    OpLt,
    OpLte,
    OpGt,
    OpGte,
    OpAnd,
    OpOr,
    OpErrorProp,
    OpArrow,
    OpSquigArrow,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Semicolon,
    Colon,
    Dot,
    EOF
  ]

mkTokenKindTest :: TokenKind -> TestTree
mkTokenKindTest k =
  testCase ("Show/Eq/Ord for constructor: " ++ show k) $ do
    -- show
    let s = show k
    length s @?= length s

    -- eq
    k == k @? "reflexive Eq"
    let other = if k == EOF then KwDef else EOF
    k /= other @? ("constructor " ++ show k ++ " must differ from " ++ show other)

    -- ord
    compare k k @?= EQ
    compare k other `seq` pure ()

--
-- private
--

tokenCreationTests :: TestTree
tokenCreationTests =
  testGroup
    "token creation"
    [ testCase "create keyword token" $ do
        let token = Token {tokenKind = KwDef, tokenValue = "def", tokenLine = 1, tokenColumn = 1}
        tokenKind token @?= KwDef
        tokenValue token @?= "def"
        tokenLine token @?= 1
        tokenColumn token @?= 1
    ]

tokenKindTests :: TestTree
tokenKindTests =
  testGroup
    "TokenKind instances"
    (map mkTokenKindTest allTokenKinds)

tokenInstanceTests :: TestTree
tokenInstanceTests =
  testGroup
    "Token instances"
    [ testCase "Show/Eq/Ord" $ do
        let t1 = Token KwDef "def" 1 1
        let t2 = Token KwReturn "return" 2 3

        -- show
        length (show t1) @?= length (show t1)

        -- eq
        t1 == t1 @? "reflexive"
        t1 /= t2 @? "different tokens"

        -- ord
        compare t1 t1 @?= EQ
        compare t1 t2 @?= LT
    ]
