{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module LdifTests where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import LDIF
import TestData
import Data.List
import qualified Data.Text as T

-- import Debug.Trace

ldifTests :: TestTree
ldifTests = testGroup "LDIF properties" [ldifPropertyTests, ldifUnitTests]

ldifPropertyTests = testGroup "LDIF properties" [
      QC.testProperty "parser is idempotent 1" ldifParserIdempotent1
    , QC.testProperty "parser is idempotent 2" ldifParserIdempotent2
    , QC.testProperty "diff is diff ldif" ldifDiffIsDiff
    , QC.testProperty "apply diff is idempotent" ldifApplyDiffIdempotent
    ]

ldifUnitTests = testGroup "LDIF unit tests" [
      testCase "apply diff 1 2" $
        ldifApplyDiff testLdif1 testLdif2 @?= True
    , testCase "apply diff 2 1" $
        ldifApplyDiff testLdif2 testLdif1 @?= True
    ]

ldifParserIdempotent1 :: LdifStr -> Bool
ldifParserIdempotent1 (LdifStr s) = srt s == srt l
    where
        l = showLdif $ parseLdif s
        srt = T.unlines . sort . T.lines

ldifParserIdempotent2 :: LdifStr -> Bool
ldifParserIdempotent2 (LdifStr s) = parseLdif s == parseLdif l
    where l = showLdif $ parseLdif s

ldifDiffIsDiff :: (LdifEntryStr, LdifEntryStr) -> Bool
ldifDiffIsDiff (LdifEntryStr s1, LdifEntryStr s2) =
        "changetype" `T.isInfixOf` showLdif dl
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

ldifApplyDiff :: LDIF -> LDIF -> Bool
ldifApplyDiff f t = either (const False) (== t) newL
    where
        newL = applyLdif dl f
        dl = diffLDIF f t
