module RewriteTests (
    rewriteTests
) where

import Test.Tasty
import Test.Tasty.HUnit
import LDAPRelay.Rewrite
import TestData
import LDIF

import Debug.Trace

rewriteTests :: TestTree
rewriteTests = testGroup "LDAPRewrite" [
      testCase "rewrite dn" $ testRewriteDN @?= True
    , testCase "rewrite attrs" $ testRewriteAttrs @?= True
    , testCase "filter attrs" $ testFilterAttrs @?= True
    ]

testRewriteDN = newDN == genLdif "dc=oof,dc=com" ldifDef1
    where
        newDN = rewriteDN [("foo", "oof"), ("com", "moc")] ldif1

testRewriteAttrs = rewritten == ldiff
    where
        rewritten = rewriteAttrs attrs ldif1
        attrs = [
            ("A", ("^A1$", "a1"))
            , ("B", ("B1", "hubba"))
            ]
        ldiff = uncurry genLdif ("dc=foo,dc=com", [
              ("A", ["a1", "A2"])
            , ("B", ["hubba"])
            ])

testFilterAttrs = filtered == ldiff
    where
        filtered = filterEntry attrs ldif1
        attrs = (".*", ["B"])
        ldiff = uncurry genLdif ("dc=foo,dc=com", [
              ("A", ["A1", "A2"])
            ])

(ldif1, ldif2) = bimap1 (uncurry genLdif) (l1, l2)
    where
        l1 = ("dc=foo,dc=com", ldifDef1)
        l2 = ("dc=bar,dc=org", ldifDef2)

