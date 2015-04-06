{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import LdifTests
import LDIF
import TestData
import EditTests
import qualified Data.Text as T

main :: IO ()
main = do
    defaultMain tests
    putStr . T.unpack $ showLdif ldiff
    where
        ldiff = genLdif "dc=foo,dc=com"
            [ ("A", ["A1"])
            , ("B", ["B1"])
            , ("C", ["C1"])
            ]

tests :: TestTree
tests = testGroup "Tests" [ldifTests, editTests]
