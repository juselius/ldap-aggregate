module RewriteTests (
    rewriteTests
) where

import Test.Tasty
import Test.Tasty.HUnit
import LDAPRelay
import TestData
import LDIF

import Debug.Trace

rewriteTests :: TestTree
rewriteTests = testGroup "LDAPRewrite" [
      testCase "rewrite dn" $ testRewriteDN @?= True
    , testCase "rewrite attrs" $ testRewriteAttrs @?= True
    , testCase "filter dn idempotency" $ testFilterDnId @?= True
    , testCase "filter dn" $ testFilterDn @?= True
    , testCase "filter attrs idempotency" $ testFilterAttrsId @?= True
    , testCase "filter attrs" $ testFilterAttrs @?= True
    ]

testRewriteDN = newDN == genLdif "dc=oof,dc=com" ldifDef1
    where
        [newDN] = rewriteDn fs [ldif1]
        fs = map makeRewriteDn [["foo", "oof"], ["com", "moc"]]

testRewriteAttrs = rewritten == ldiff
    where
        [rewritten] = rewriteAttrs attrs [ldif1]
        attrs = map makeRewriteAttrs [
            ["A", "^A1$", "a1"]
            , ["B", "B1", "hubba"]
            ]
        ldiff = uncurry genLdif ("dc=foo,dc=com", [
              ("A", ["a1", "A2"])
            , ("B", ["hubba"])
            ])

testFilterDnId = filtered == ldiff
    where
        [filtered] = filterDn attrs [ldif1]
        attrs = map makeDnFilter []
        ldiff = uncurry genLdif ("dc=foo,dc=com",
            [ ("A", ["A1", "A2"])
            , ("B", ["B1"])
            ])

testFilterDn = filtered == ldiff
    where
        [filtered] = filterDn attrs [ldif1, ldif2]
        attrs = map makeDnFilter ["bar"]
        ldiff = uncurry genLdif ("dc=foo,dc=com",
            [ ("A", ["A1", "A2"])
            , ("B", ["B1"])
            ])

testFilterAttrsId = filtered == ldiff
    where
        [filtered] = filterEntries attrs [ldif1]
        attrs = map makeAttrFilter []
        ldiff = uncurry genLdif ("dc=foo,dc=com",
            [ ("A", ["A1", "A2"])
            , ("B", ["B1"])
            ])

testFilterAttrs =
    trace (unwords
    ["\n", showLDIF filtered , "\nvs.\n", showLDIF ldiff])
    filtered == ldiff
    where
        [filtered] = filterEntries attrs [ldif1]
        attrs = map makeAttrFilter [["", "A", "A2"]]
        ldiff = uncurry genLdif ("dc=foo,dc=com",
            [ ("A", ["A1"])
            , ("B", ["B1"])
            ])

(ldif1, ldif2) = bimap1 (uncurry genLdif) (l1, l2)
    where
        l1 = ("dc=foo,dc=com", ldifDef1)
        l2 = ("dc=bar,dc=org", ldifDef2)

