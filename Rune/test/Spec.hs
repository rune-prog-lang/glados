import AST.NodesSpec (astNodesTests)
import AST.ParserSpec (astParserTests)
import CLISpec (cliTests)
import Lexer.LexerSpec (lexerTests)
import Lexer.TokensSpec (tokensTests)
import LoggerSpec (loggerTests)
import PipelinesSpec (pipelinesTests)
import Semantics.VarsSpec (varsSemanticsTests)
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "All Tests"
      [ varsSemanticsTests,
        cliTests,
        lexerTests,
        pipelinesTests,
        loggerTests,
        astNodesTests,
        astParserTests,
        tokensTests
      ]
