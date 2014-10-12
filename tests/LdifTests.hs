{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module LdifTests where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import LDIF
import TestData
import Data.List
import Control.Arrow (second)
import qualified Data.ByteString.Char8 as BS

import Debug.Trace

ldifTests :: TestTree
ldifTests = testGroup "LDIF properties" [ldifPropertyTests, ldifUnitTests]

ldifPropertyTests = testGroup "LDIF properties" [
      QC.testProperty "ldif parser is idempotent" ldifParserIdempotent
    , QC.testProperty "ldif diff is diff" ldifDiffIsDiff
    , QC.testProperty "ldif apply diff idempotent" ldifApplyDiffIdempotent
    ]

ldifUnitTests = testGroup "LDIF unit tests" [
      testCase "ldif apply diff 1" $ ldifApplyDiff1 @?= True
    , testCase "ldif apply diff 2" $ ldifApplyDiff2 @?= True
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

ldifApplyDiff1 :: Bool
ldifApplyDiff1 =
        either (const False) (== [testLdif1]) newLdif
        where
            newLdif = applyLdif dl [testLdif2]
            dl = diffLDIF [testLdif2] [testLdif1]

ldifApplyDiff2 :: Bool
ldifApplyDiff2 =
        either (const False) (== [testLdif2]) newLdif
        where
            newLdif = applyLdif dl [testLdif1]
            dl = diffLDIF [testLdif1] [testLdif2]

