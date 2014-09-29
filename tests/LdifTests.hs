{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module LdifTests where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import LDIF
import TestData
import qualified Data.ByteString.Char8 as BS

ldifTests :: TestTree
ldifTests = testGroup "LDIF tests" [ldifPropertyTests, ldifUnitTests]

ldifPropertyTests = testGroup "LDIF properties" [
      QC.testProperty "parse ldif string 1" ptest1
    ]

ldifUnitTests = testGroup "LDIF unit tests" [
      testCase "parse ldif string 1" $ test1 @?= True
    , testCase "parse ldif string 2" $ test2 @?= True
    , testCase "ldif1 /= ldif2" $ test3 @?= True
    , testCase "diff 1 2 == 2" $ test4 @?= True
    , testCase "diff 2 1 == 1" $ test5 @?= True
    ]

ptest1 :: LdifEntryStr -> Bool
ptest1 (LdifEntryStr s) = s == l
    where
        l = init . unlines . map show $ parseLdif $ BS.pack s

test1 = (head ldif1) == uncurry genLdif l1
test2 = (head ldif2) == uncurry genLdif l2
test3 = (head ldif1) /= uncurry genLdif l2
test4 = diffLDIF ldif1 ldif2 == ldiff1
test5 = diffLDIF ldif2 ldif1 == ldiff2

l1 = ("A", ldifDef1)
l2 = ("B", ldifDef2)
(ldif1, ldif2) = bimap1 (parseLdif . uncurry genLdif') (l1, l2)

