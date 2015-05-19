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
    c <- readConfig "tests/fromto.yml"
    s <- toLdif $ readFile "tests/ldif/from.ldif"
    t <- toLdif $ readFile "tests/ldif/to.ldif"
    let r = getLdifRules . head $ sourceDITs c
    applyLdifRules r s @?= t
    where
        toLdif = fmap $ lRec . parseLdif . T.pack

