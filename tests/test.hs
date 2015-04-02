{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import LdifTests
import SimpleLDIF
import TestData
import qualified Data.Text as T
--import RewriteTests

main :: IO ()
main = do
    let ldiff = genLdif "dc=foo,dc=com"
            [ ("A", ["A1"])
            , ("B", ["B1"])
            , ("C", ["C1"])
            ]
    putStr . T.unpack $ showLdif ldiff
    defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ldifTests] --, rewriteTests]
