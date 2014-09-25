module RelayTests (
    relayTests
) where

import Test.Tasty
import Test.Tasty.HUnit

relayTests :: TestTree
relayTests = testGroup "Relay unit tests" [
      testCase "1" $ True @?= True
    , testCase "2" $ GT @?= GT
    ]


