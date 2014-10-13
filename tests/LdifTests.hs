{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module LdifTests where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import LDIF
import TestData
import Data.List
import Control.Arrow (second)
import qualified Data.HashMap.Lazy as HM
import qualified Data.ByteString.Char8 as BS

import Debug.Trace

ldifTests :: TestTree
ldifTests = testGroup "LDIF properties" [ldifPropertyTests, ldifUnitTests]

ldifPropertyTests = testGroup "LDIF properties" [
      QC.testProperty "ldif parser is idempotent" ldifParserIdempotent
    , QC.testProperty "ldif diff is diff ldif" ldifDiffIsDiff
    , QC.testProperty "ldif apply diff is idempotent" ldifApplyDiffIdempotent
    , QC.testProperty "test isUniqueDN" testIsUniqueDN
    , QC.testProperty "test toHashMap" testToHashMap
    , QC.testProperty "ldif lifting" testLiftLdif
    ]

ldifUnitTests = testGroup "LDIF unit tests" [
      testCase "ldif apply diff 1 2" $ ldifApplyDiff12 @?= True
    , testCase "ldif apply diff 2 1" $ ldifApplyDiff21 @?= True
    ]

ldifParserIdempotent:: LdifStr -> Bool
ldifParserIdempotent (LdifStr s) = s == l
    where
        l = init . unlines . map showLDIF $ parseLdif $ BS.pack s

ldifDiffIsDiff :: (LdifEntryStr, LdifEntryStr) -> Bool
ldifDiffIsDiff (LdifEntryStr s1, LdifEntryStr s2) =
        "changetype" `isInfixOf` (unlines . map showLDIF $ dl)
        where
            l1 = parseLdif $ BS.pack s1
            l2 = parseLdif $ BS.pack s2
            dl = diffLDIF l1 l2

ldifApplyDiffIdempotent :: (LdifEntryStr, LdifEntryStr) -> Bool
ldifApplyDiffIdempotent (LdifEntryStr s1, LdifEntryStr s2) =
        either (const False) (== l1) $ applyLdif dl l2
        where
            l1 = parseLdif $ BS.pack s1
            l2 = parseLdif $ BS.pack s2
            dl = diffLDIF l2 l1

ldifApplyDiff12 :: Bool
ldifApplyDiff12 =
        either (const False) (== [testLdif1]) newLdif
        where
            newLdif = applyLdif dl [testLdif2]
            dl = diffLDIF [testLdif2] [testLdif1]

ldifApplyDiff21 :: Bool
ldifApplyDiff21 =
        either (const False) (== [testLdif2]) newLdif
        where
            newLdif = applyLdif dl [testLdif1]
            dl = diffLDIF [testLdif1] [testLdif2]

testIsUniqueDN :: LdifStr -> Bool
testIsUniqueDN (LdifStr s) = isUniqDN l
    where
        l = parseLdif $ BS.pack s

testToHashMap :: LdifStr -> Bool
testToHashMap (LdifStr s) = l == HM.toList (toHashMap l)
    where
        l = parseLdif $ BS.pack s

testLiftLdif :: LdifStr -> Property
testLiftLdif (LdifStr s) = not (null s) ==>
    (not (not (null l2) && not (null n2)) || l2 /= n2)
    where
        n = map (second (liftLdif (map f))) l
        f (a, v) = (a, ["abcdefghijklmnopqrstuvxyz"])
        l = parseLdif $ BS.pack s
        (_, l2) = partition isDel (map snd l)
        (_, n2) = partition isDel (map snd n)
        isDel LDIFDelete = True
        isDel _ = False
