module ParserLibSpec (spec) where

import ParserLib
import Test.Tasty
import Test.Tasty.HUnit
import Type (Parser (runParser))

shouldParseTo :: (Eq a, Show a) => Either String (a, String) -> Either String (a, String) -> Assertion
shouldParseTo = (@?=)

spec :: TestTree
spec =
  testGroup
    "ParserLib Tests"
    [ testParseChar,
      testParseAnyChar,
      testParseOr,
      testParseAnd,
      testParseMany,
      testParseSome,
      testParseUInt,
      testParseInt,
      testParseDouble,
      testParseString,
      testParseAnyCharNot,
      testSepBy
    ]

testParseChar :: TestTree
testParseChar =
  testGroup
    "parseChar"
    [ testCase "parses matching character" $
        runParser (parseChar 'a') "abc" `shouldParseTo` Right ('a', "bc"),
      testCase "fails on non-matching character" $
        runParser (parseChar 'a') "bac" `shouldParseTo` Left "Not Found",
      testCase "fails on empty input" $
        runParser (parseChar 'a') "" `shouldParseTo` Left "Empty String"
    ]

testParseAnyChar :: TestTree
testParseAnyChar =
  testGroup
    "parseAnyChar"
    [ testCase "parses first matching character" $
        runParser (parseAnyChar "ab") "ax" `shouldParseTo` Right ('a', "x"),
      testCase "parses second matching character" $
        runParser (parseAnyChar "ab") "bx" `shouldParseTo` Right ('b', "x"),
      testCase "fails on no match" $
        runParser (parseAnyChar "ab") "cx" `shouldParseTo` Left "Not Found"
    ]

testParseOr :: TestTree
testParseOr =
  testGroup
    "parseOr"
    [ testCase "first parser succeeds" $
        runParser (parseChar 'a' `parseOr` parseChar 'b') "abc" `shouldParseTo` Right ('a', "bc"),
      testCase "second parser succeeds" $
        runParser (parseChar 'a' `parseOr` parseChar 'b') "bbc" `shouldParseTo` Right ('b', "bc"),
      testCase "both parsers fail" $
        runParser (parseChar 'a' `parseOr` parseChar 'b') "cbc" `shouldParseTo` Left "Not Found"
    ]

testParseAnd :: TestTree
testParseAnd =
  testGroup
    "parseAnd"
    [ testCase "both parsers succeed" $
        runParser (parseChar 'a' `parseAnd` parseChar 'b') "abc" `shouldParseTo` Right (('a', 'b'), "c"),
      testCase "first parser fails" $
        runParser (parseChar 'x' `parseAnd` parseChar 'b') "abc" `shouldParseTo` Left "Not Found",
      testCase "second parser fails" $
        runParser (parseChar 'a' `parseAnd` parseChar 'x') "abc" `shouldParseTo` Left "Not Found"
    ]

testParseMany :: TestTree
testParseMany =
  testGroup
    "parseMany"
    [ testCase "parses multiple characters" $
        runParser (parseMany (parseChar 'a')) "aaabc" `shouldParseTo` Right ("aaa", "bc"),
      testCase "parses one character" $
        runParser (parseMany (parseChar 'a')) "abc" `shouldParseTo` Right ("a", "bc"),
      testCase "parses zero characters (succeeds)" $
        runParser (parseMany (parseChar 'a')) "bc" `shouldParseTo` Right ("", "bc")
    ]

testParseSome :: TestTree
testParseSome =
  testGroup
    "parseSome"
    [ testCase "parses multiple characters" $
        runParser (parseSome (parseChar 'a')) "aaabc" `shouldParseTo` Right ("aaa", "bc"),
      testCase "parses one character" $
        runParser (parseSome (parseChar 'a')) "abc" `shouldParseTo` Right ("a", "bc"),
      testCase "fails on zero characters" $
        runParser (parseSome (parseChar 'a')) "bc" `shouldParseTo` Left "Not Found"
    ]

testParseUInt :: TestTree
testParseUInt =
  testGroup
    "parseUInt"
    [ testCase "parses an unsigned integer" $
        runParser parseUInt "123x" `shouldParseTo` Right (123, "x"),
      testCase "stops at non-digit" $
        runParser parseUInt "45 6" `shouldParseTo` Right (45, " 6")
    ]

testParseInt :: TestTree
testParseInt =
  testGroup
    "parseInt"
    [ testCase "parses a positive integer" $
        runParser parseInt "123x" `shouldParseTo` Right (123, "x"),
      testCase "parses a negative integer" $
        runParser parseInt "-45y" `shouldParseTo` Right (-45, "y"),
      testCase "parses zero" $
        runParser parseInt "0z" `shouldParseTo` Right (0, "z")
    ]

testParseDouble :: TestTree
testParseDouble =
  testGroup
    "parseDouble"
    [ testCase "parses a positive double" $
        runParser parseDouble "123.45a" `shouldParseTo` Right (123.45, "a"),
      testCase "parses a negative double" $
        runParser parseDouble "-5.25b" `shouldParseTo` Right (-5.25, "b"),
      testCase "parses double starting with zero" $
        runParser parseDouble "0.99c" `shouldParseTo` Right (0.99, "c")
    ]

testParseString :: TestTree
testParseString =
  testGroup
    "parseString"
    [ testCase "parses an exact string match" $
        runParser (parseString "hello") "hello world" `shouldParseTo` Right ("hello", " world"),
      testCase "fails on partial match" $
        runParser (parseString "hello") "hell no" `shouldParseTo` Left "Not Found",
      testCase "fails on no match" $
        runParser (parseString "hello") "world" `shouldParseTo` Left "Not Found"
    ]

testParseAnyCharNot :: TestTree
testParseAnyCharNot =
  testGroup
    "parseAnyCharNot"
    [ testCase "parses a valid char" $
        runParser (parseAnyCharNot "ab") "cde" `shouldParseTo` Right ('c', "de"),
      testCase "fails on forbidden char" $
        runParser (parseAnyCharNot "ab") "ade" `shouldParseTo` Left "Char not allowed: a",
      testCase "fails on another forbidden char" $
        runParser (parseAnyCharNot "ab") "bde" `shouldParseTo` Left "Char not allowed: b",
      testCase "fails on empty input" $
        runParser (parseAnyCharNot "ab") "" `shouldParseTo` Left "Unexpected end"
    ]

testSepBy :: TestTree
testSepBy =
  testGroup
    "sepBy"
    [ testCase "parses multiple items with separator" $
        runParser (parseWord `sepBy` parseChar ',') "word1,word2,word3" `shouldParseTo` Right (["word1", "word2", "word3"], ""),
      testCase "parses a single item" $
        runParser (parseWord `sepBy` parseChar ',') "word1" `shouldParseTo` Right (["word1"], ""),
      testCase "parses zero items (succeeds)" $
        runParser (parseWord `sepBy` parseChar ',') "" `shouldParseTo` Right ([], ""),
      testCase "handles trailing separator" $
        runParser (parseWord `sepBy` parseChar ',') "word1," `shouldParseTo` Right (["word1"], ",")
    ]
