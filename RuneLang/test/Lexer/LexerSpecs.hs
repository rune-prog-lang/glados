module Lexer.LexerSpecs (lexerTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

import Rune.Lexer.Lexer (lexer)
import Rune.Lexer.Tokens (Token (..), TokenKind (..))

--
-- public
--

lexerTests :: TestTree
lexerTests =
  testGroup
    "Rune Lexer Component Tests"
    [ mainLexerTests
    ]

--
-- helpers
--

tokP :: TokenKind -> String -> Int -> Int -> Token
tokP = Token

lexTest :: String -> [Token] -> IO ()
lexTest input expected =
  case lexer "test.rune" input of
    Right actual -> assertEqual ("Lexing: " ++ show input) expected actual
    Left err -> assertFailure $ "Lexing failed unexpectedly: " ++ show err

lexFailTest :: String -> IO ()
lexFailTest input =
  case lexer "test.rune" input of
    Left _ -> return ()
    Right tokens -> assertFailure $ "Expected lexer failure on input: " ++ show input ++ ", but got tokens: " ++ show tokens

--
-- private
--

test_empty_input :: TestTree
test_empty_input = testCase "Empty input" $
  lexTest "" [tokP EOF "" 1 1]

test_whitespace_only :: TestTree
test_whitespace_only = testCase "Whitespace only" $
  lexTest " \t\n" [tokP EOF "" 2 1]

test_single_line_comment :: TestTree
test_single_line_comment = testCase "Single line comment" $
  lexTest "// comment\n" [tokP EOF "" 2 1]

test_block_comment :: TestTree
test_block_comment = testCase "Block comment" $
  lexTest "/* multi\nline\ncomment */" [tokP EOF "" 3 11]

test_mixed_comments_and_tokens :: TestTree
test_mixed_comments_and_tokens = testCase "Comments, Whitespace and Tokens" $
  lexTest "/*block*/ 123 //line\n\n\t def"
    [ tokP (LitInt 123) "123" 1 11,
      tokP (KwDef) "def" 3 10,
      tokP EOF "" 3 13 ]

test_full_snippet_position :: TestTree
test_full_snippet_position = testCase "Full snippet and position check" $
  lexTest "def main() -> i32 {\n  return 0;\n}"
    [ tokP (KwDef) "def" 1 1,
      tokP (Identifier "main") "main" 1 5,
      tokP LParen "(" 1 9,
      tokP RParen ")" 1 10,
      tokP OpArrow "->" 1 12,
      tokP TypeI32 "i32" 1 15,
      tokP LBrace "{" 1 19,
      tokP KwReturn "return" 2 3,
      tokP (LitInt 0) "0" 2 10,
      tokP Semicolon ";" 2 11,
      tokP RBrace "}" 3 1,
      tokP EOF "" 3 2 ]

test_invalid_character :: TestTree
test_invalid_character = testCase "Invalid character failure" $
  lexFailTest "@"

mainLexerTests :: TestTree
mainLexerTests =
  testGroup
    "Lexer Core (Integration, Comments, Position)"
    [ test_empty_input,
      test_whitespace_only,
      test_single_line_comment,
      test_block_comment,
      test_mixed_comments_and_tokens,
      test_full_snippet_position,
      test_invalid_character
    ]
