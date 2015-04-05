{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import LdifTests
import SimpleLDIF
import TestData
-- import RewriteTests
import qualified Data.Text as T

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
