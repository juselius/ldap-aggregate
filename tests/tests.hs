import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.Char

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties"
    [ QC.testProperty "example 1" xmpl
    , QC.testProperty "example 2" xmpl'
    ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT

  -- the following test does not hold
  , testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1,2,2] @?= LT
  ]

xmpl = ((\s -> (reverse.reverse) s == s) :: [Char] -> Bool)

xmpl' = ((\s -> length s < 3) :: [Integer] -> Bool)

