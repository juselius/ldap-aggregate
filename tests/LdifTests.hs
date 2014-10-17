{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module LdifTests where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import LDIF
import TestData
import Data.List
import Control.Arrow (second)
import qualified Data.HashMap.Lazy as M
import qualified Data.ByteString.Char8 as BS

import Debug.Trace

ldifTests :: TestTree
ldifTests = testGroup "LDIF properties" [ldifPropertyTests, ldifUnitTests]

ldifPropertyTests = testGroup "LDIF properties" [
      QC.testProperty "parser is idempotent" ldifParserIdempotent
    , QC.testProperty "diff is diff ldif" ldifDiffIsDiff
    , QC.testProperty "apply diff is idempotent" ldifApplyDiffIdempotent
    , QC.testProperty "lifting" testLiftLdif
    ]

ldifUnitTests = testGroup "LDIF unit tests" [
      testCase "apply diff 1 2" $ ldifApplyDiff12 @?= True
    , testCase "apply diff 2 1" $ ldifApplyDiff21 @?= True
    ]

ldifParserIdempotent:: LdifStr -> Bool
ldifParserIdempotent (LdifStr s) = srt s == srt l
    where
        l = showLdif $ parseLdif $ BS.pack s
        srt = unlines . sort . lines

ldifDiffIsDiff :: (LdifEntryStr, LdifEntryStr) -> Bool
ldifDiffIsDiff (LdifEntryStr s1, LdifEntryStr s2) =
        "changetype" `isInfixOf` (showLdif $ dl)
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
            dbg = "\ndbg: " ++ "\n"
                ++ (showLdif l1) ++ "\n"
                ++ (showLdif l2) ++ "\n"
                ++ (showLdif dl) ++ "\n"
                ++ (either ("fan! " ++) showLdif (applyLdif dl l2)) ++ "\n"

ldifApplyDiff12 :: Bool
ldifApplyDiff12 =
        either (const False) (== testLdif1) newLdif
        where
            newLdif = applyLdif dl testLdif2
            dl = diffLDIF testLdif2 testLdif1

ldifApplyDiff21 :: Bool
ldifApplyDiff21 =
        either (const False) (== testLdif2) newLdif
        where
            newLdif = applyLdif dl testLdif1
            dl = diffLDIF testLdif1 testLdif2

testLiftLdif :: LdifStr -> Property
testLiftLdif (LdifStr s) = not (null s) ==>
    (not (not (M.null l) && not (M.null l')) || l /= l')
    where
        ldif = parseLdif $ BS.pack s
        ldif' = mapLDIF f ldif
        l = M.filter isDel ldif
        l' = ldif `M.difference` l
        f _ v = "abcdefghijklmnopqrstuvxyz"
        isDel (LDIFDelete _) = True
        isDel _ = False
