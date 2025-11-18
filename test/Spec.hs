import ParserLibSpec (spec)
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "ParserLib Tests" [ParserLibSpec.spec]
