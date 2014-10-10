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
ldifTests = testGroup "LDIF properties" [ldifPropertyTests]

ldifPropertyTests = testGroup "LDIF properties" [
      QC.testProperty "ldif parser is idempotent" ldifParserIdempotent
    , QC.testProperty "ldif diff is diff" ldifDiffIsDiff
    , QC.testProperty "ldif apply diff is idempotent" ldifApplyDiffIdempotent
    ]

ldifParserIdempotent:: LdifStr -> Bool
ldifParserIdempotent (LdifStr s) = s == l
    where
        l = init . unlines . map show $ parseLdif $ BS.pack s

ldifDiffIsDiff :: (LdifEntryStr, LdifEntryStr) -> Bool
ldifDiffIsDiff (LdifEntryStr s1, LdifEntryStr s2) =
        "changetype" `isInfixOf` (unlines . map show $ dl)
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

