import Test.Tasty
import LdifTests
import RewriteTests

main :: IO ()
main = do
    defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ldifTests, rewriteTests]
