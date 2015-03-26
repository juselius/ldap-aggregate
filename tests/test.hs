import Test.Tasty
import LdifTests
import LDIF
import TestData
--import RewriteTests

main :: IO ()
main = do
    let ldiff = genLdif "dc=foo,dc=com"
            [ ("A", ["A1"])
            , ("B", ["B1"])
            , ("C", ["C1"])
            ]
    putStr $ showLdif ldiff
    defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ldifTests] --, rewriteTests]
