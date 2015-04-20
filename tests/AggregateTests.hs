{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module AggregateTests (
    aggTests
) where

import Test.Tasty
import Test.Tasty.HUnit
import LDIF
import Editor
import Config
import DITs
import qualified Data.Text as T

import Debug.Trace

aggTests :: TestTree
aggTests =
    testGroup "Aggreagate DITs" [
      testCase "aggregate" $ testAgg
      ]

testAgg :: Assertion
testAgg = do
    c <- readConfig "tests/test.yml"
    s <- toLdif $ readFile "tests/source.ldif"
    t <- toLdif $ readFile "tests/target.ldif"
    let ign = map doIgnore  $ ignoreFilters  . head $ sourceDIT c
        rw  = map doRewrite $ rewriteFilters . head $ sourceDIT c
    runEdits rw (runEdits ign s) @?= t
    s @?= t
    where
        toLdif = fmap $ lRec . parseLdif . T.pack

