{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import LdifTests
import EditTests
import DITTests
import AggregateTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
      ldifTests
    , editTests
    , ditTests
    , aggTests
    ]
