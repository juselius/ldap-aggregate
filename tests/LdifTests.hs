--module LdifTests (
    --ldifTests
--) where

module LdifTests where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Arrow (first)
import qualified Data.ByteString.Char8 as BS
import LDIF

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
(ldif1, ldif2) = bimap1 (parseLdif . uncurry genLdifText) (l1, l2)

ldifDef1 :: [AttrSpec]
ldifDef1 = [
      ("A", ["A1", "A2"])
    , ("B", ["B1"])
    ]

ldifDef2 :: [AttrSpec]
ldifDef2 = [
      ("A", ["A1"])
    ]

ldiff1 :: [LDIF]
ldiff1 = [
      LDIF ("B", LDIFAdd [LDAPMod LdapModAdd "A" ["A1"]])
    , LDIF ("A", LDIFDelete)
    ]

ldiff2 :: [LDIF]
ldiff2 = [
      LDIF ("A", LDIFAdd [ LDAPMod LdapModAdd "A" ["A1", "A2"]
                         , LDAPMod LdapModAdd "B" ["B1"]
                         ]
           )
    , LDIF ("B", LDIFDelete)
    ]

genLdifText :: String -> [AttrSpec] -> BS.ByteString
genLdifText dn av = BS.pack $ "dn: " ++ dn ++ "\n" ++ attrs
    where
        attrs = foldl (\s (a, v) ->
            s ++ a ++ ": " ++ v ++ "\n") "" (expanded av)
        expanded = concat . map (uncurry zip . first repeat)

genLdif :: String -> [AttrSpec] -> LDIF
genLdif dn av = LDIF (dn, LDIFEntry $ LDAPEntry dn av)

