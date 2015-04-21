{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module AggregateTests (
    aggTests
) where

import Test.Tasty
import Test.Tasty.HUnit
import LDIF
import Config
import DITs
import qualified Data.Text as T

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
    let r = getLdifRules . head $ sourceDIT c
    applyLdifRules r s @?= t
    where
        toLdif = fmap $ lRec . parseLdif . T.pack

