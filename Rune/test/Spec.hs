import AST.NodesSpec (astNodesTests)
import AST.ParserSpec (astParserTests)
import AST.ProgramSyntaxSpec (programSyntaxTests)
import CLISpec (cliTests)
import Lexer.LexerSpec (lexerTests)
import Lexer.TokensSpec (tokensTests)
import LoggerSpec (loggerTests)
import PipelinesSpec (pipelinesTests)
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "All Tests"
      [ cliTests,
        lexerTests,
        pipelinesTests,
        loggerTests,
        astNodesTests,
        astParserTests,
        tokensTests,
        programSyntaxTests
      ]
