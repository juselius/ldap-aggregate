--module LdifTests (
    --ldifTests
--) where

module LdifTests where

import Test.Tasty
import Test.Tasty.HUnit
import LDIF
import TestData

ldifTests :: TestTree
ldifTests = testGroup "LDIF unit tests" [
      testCase "parse ldif string 1" $ test1 @?= True
    , testCase "parse ldif string 2" $ test2 @?= True
    , testCase "ldif1 /= ldif2" $ test3 @?= True
    , testCase "diff 1 2 == 2" $ test4 @?= True
    , testCase "diff 2 1 == 1" $ test5 @?= True
    ]

test1 = (head ldif1) == uncurry genLdif l1
test2 = (head ldif2) == uncurry genLdif l2
test3 = (head ldif1) /= uncurry genLdif l2
test4 = diffLDIF ldif1 ldif2 == ldiff1
test5 = diffLDIF ldif2 ldif1 == ldiff2

l1 = ("A", ldifDef1)
l2 = ("B", ldifDef2)
(ldif1, ldif2) = bimap1 (parseLdif . uncurry genLdif') (l1, l2)

