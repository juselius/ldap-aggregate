{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module LdifTests where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import SimpleLDIF
import TestData
import Data.List
import Control.Arrow (second)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T

import Debug.Trace

ldifTests :: TestTree
ldifTests = testGroup "LDIF properties" [ldifPropertyTests, ldifUnitTests]

ldifPropertyTests = testGroup "LDIF properties" [
      QC.testProperty "parser is idempotent" ldifParserIdempotent
    , QC.testProperty "diff is diff ldif" ldifDiffIsDiff
    , QC.testProperty "apply diff is idempotent" ldifApplyDiffIdempotent
    ]

ldifUnitTests = testGroup "LDIF unit tests" [
      testCase "apply diff 1 2" $ ldifApplyDiff12 @?= True
    , testCase "apply diff 2 1" $ ldifApplyDiff21 @?= True
    ]

ldifParserIdempotent:: LdifStr -> Bool
ldifParserIdempotent (LdifStr s) = srt s == srt l
    where
        l = showLdif $ parseLdif $ s
        srt = T.unlines . sort . T.lines

ldifDiffIsDiff :: (LdifEntryStr, LdifEntryStr) -> Bool
ldifDiffIsDiff (LdifEntryStr s1, LdifEntryStr s2) =
        "changetype" `T.isInfixOf` (showLdif $ dl)
        where
            l1 = parseLdif s1
            l2 = parseLdif s2
            dl = diffLDIF l1 l2

ldifApplyDiffIdempotent :: (LdifEntryStr, LdifEntryStr) -> Bool
ldifApplyDiffIdempotent (LdifEntryStr s1, LdifEntryStr s2) =
        either (const False) (== l1) $ applyLdif dl l2
        where
            l1 = parseLdif s1
            l2 = parseLdif s2
            dl = diffLDIF l2 l1
            dbg = "\ndbg: " `T.append` "\n"
                `T.append` (showLdif l1) `T.append` "\n"
                `T.append` (showLdif l2) `T.append` "\n"
                `T.append` (showLdif dl) `T.append` "\n"
                `T.append` (either (\x -> "oops! " `T.append` T.pack x)
                    showLdif (applyLdif dl l2)) `T.append` "\n"

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

