module RewriteTests (
    rewriteTests
) where

import Test.Tasty
import Test.Tasty.HUnit
import LDAPRelay.Rewrite
import TestData
import LDIF.Utils (bimap1)

rewriteTests :: TestTree
rewriteTests = testGroup "Rewrite unit tests" [
      testCase "rewriteDN" $ test1 @?= True
    , testCase "rewriteAttrList" $ test2 @?= True
    , testCase "fitlerAttrs" $ test3 @?= True
    ]

test1 = newDN == genLdif "dc=oof,dc=moc" ldifDef1
test2 = False
test3 = False

newDN = rewriteDN [("foo", "oof"), ("com", "moc")] ldif1

l1 = ("dc=foo,dc=com", ldifDef1)
l2 = ("dc=bar,dc=org", ldifDef2)

(ldif1, ldif2) = bimap1 (uncurry genLdif) (l1, l2)
