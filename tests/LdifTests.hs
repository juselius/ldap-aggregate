{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module LdifTests where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import LDIF
import TestData
import Data.List
import qualified Data.ByteString.Char8 as BS

import Debug.Trace

ldifTests :: TestTree
ldifTests = testGroup "LDIF tests" [ldifPropertyTests, ldifUnitTests]

ldifPropertyTests = testGroup "LDIF properties" [
      QC.testProperty "ldif parser is idempotent" ldif_parser_idempotent
    , QC.testProperty "ldif diff is diff" ldif_diff_is_diff
    , QC.testProperty "ldif apply diff is idempotent" ldif_apply_diff_idempotent
    ]

ldifUnitTests = testGroup "LDIF unit tests" [
      testCase "parse ldif string 1" $ test1 @?= True
    , testCase "parse ldif string 2" $ test2 @?= True
    , testCase "ldif1 /= ldif2" $ test3 @?= True
    , testCase "diff 1 2 == 2" $ test4 @?= True
    , testCase "diff 2 1 == 1" $ test5 @?= True
    ]

ldif_parser_idempotent:: LdifEntryStr -> Bool
ldif_parser_idempotent (LdifEntryStr s) = s == l
    where
        l = init . unlines . map show $ parseLdif $ BS.pack s

ldif_diff_is_diff :: (LdifEntryStr, LdifEntryStr) -> Property
ldif_diff_is_diff (LdifEntryStr s1, LdifEntryStr s2) =
    not ("changetype" `isInfixOf` s1) ==>
    not ("changetype" `isInfixOf` s2) ==>
        "changetype" `isInfixOf` (unlines . map show $ dl)
        where
            l1 = parseLdif $ BS.pack s1
            l2 = parseLdif $ BS.pack s2
            dl = diffLDIF l1 l2

ldif_apply_diff_idempotent :: (LdifEntryStr, LdifEntryStr) -> Property
ldif_apply_diff_idempotent (LdifEntryStr s1, LdifEntryStr s2) =
    not ("changetype" `isInfixOf` s1) ==>
    not ("changetype" `isInfixOf` s2) ==>
        either (const False) (== l1) $ applyLDIF dl l2
        where
            l1 = parseLdif $ BS.pack s1
            l2 = parseLdif $ BS.pack s2
            dl = diffLDIF l2 l1

test1 = (head ldif1) == uncurry genLdif l1'
test2 = (head ldif2) == uncurry genLdif l2'
test3 = (head ldif1) /= uncurry genLdif l2'
test4 = diffLDIF ldif1 ldif2 == ldiff1
test5 = diffLDIF ldif2 ldif1 == ldiff2

l1' = ("A", ldifDef1)
l2' = ("B", ldifDef2)
(ldif1, ldif2) = bimap1 (parseLdif . uncurry genLdif') (l1', l2')

