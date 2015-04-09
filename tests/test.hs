{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import LdifTests
import EditTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
      ldifTests
    , editTests
    ]
