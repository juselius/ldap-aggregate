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

testRewriteDN = newDN == genLdif "dc=oof,dc=com" ldifAttrs1
    where
        [newDN] = rewriteDn fs [testLdif1]
        fs = map makeRewriteDn [["foo", "oof"], ["com", "moc"]]

testRewriteAttrs = rewritten == ldiff
    where
        [rewritten] = rewriteAttrs attrs [testLdif1]
        attrs = map makeRewriteAttrs [
            ["A", "^A1$", "a1"]
            , ["B", "B1", "hubba"]
            ]
        ldiff = genLdif "dc=foo,dc=com"
            [ ("A", ["a1", "A2"])
            , ("B", ["hubba"])
            , ("C", ["C1"])
            ]

testFilterDnId = filtered == ldiff
    where
        [filtered] = filterDn attrs [testLdif1]
        attrs = map makeDnFilter []
        ldiff = genLdif "dc=foo,dc=com"
            [ ("A", ["A1", "A2"])
            , ("B", ["B1"])
            , ("C", ["C1"])
            ]

testFilterDn = filtered == ldiff
    where
        [filtered] = filterDn attrs [testLdif1, testLdif1']
        attrs = map makeDnFilter ["bar"]
        ldiff = genLdif "dc=foo,dc=com"
            [ ("A", ["A1", "A2"])
            , ("B", ["B1"])
            , ("C", ["C1"])
            ]

testFilterAttrsId = filtered == ldiff
    where
        [filtered] = filterEntries attrs [testLdif1]
        attrs = map makeAttrFilter []
        ldiff = genLdif "dc=foo,dc=com"
            [ ("A", ["A1", "A2"])
            , ("B", ["B1"])
            , ("C", ["C1"])
            ]

testFilterAttrs =
    --trace (unwords
    --["\n", showLDIF filtered , "\nvs.\n", showLDIF ldiff])
    filtered == ldiff
    where
        [filtered] = filterEntries attrs [testLdif1]
        attrs = map makeAttrFilter [["", "A", "A2"]]
        ldiff = genLdif "dc=foo,dc=com"
            [ ("A", ["A1"])
            , ("B", ["B1"])
            , ("C", ["C1"])
            ]

