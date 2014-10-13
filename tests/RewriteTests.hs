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
      testCase "filter dn idempotency" $ testFilterDnId @?= True
    , testCase "filter dn" $ testFilterDn @?= True
    , testCase "filter attrs idempotency" $ testFilterAttrsId @?= True
    , testCase "filter attrs" $ testFilterAttrs @?= True
    , testCase "filter dn attrs" $ testFilterDnAttrs @?= True
    , testCase "filter attr vals" $ testFilterAttrVals @?= True
    , testCase "filter ldif" $ testFilterLdif @?= True
    , testCase "rewrite dn 1" $ testRewriteDN1 @?= True
    , testCase "rewrite dn 2" $ testRewriteDN2 @?= True
    , testCase "rewrite attr name" $ testRewriteAttrName @?= True
    , testCase "rewrite attrs" $ testRewriteAttrs @?= True
    , testCase "rewrite dn attrs" $ testRewriteDnAttrs @?= True
    , testCase "replace attrs" $ testReplaceAttrs @?= True
    ]

testRewriteDN1 = newDN == genLdif "dc=oof,dc=com" ldifAttrs1
    where
        [newDN] = rewriteDn fs [testLdif1]
        fs = map makeRewriteDn [["foo", "oof"], ["com", "moc"]]

testRewriteDN2 = newDN == genLdif "dc=oof,dc=com" ldifAttrs2
    where
        [newDN] = rewriteDn fs [testLdif2]
        fs = map makeRewriteDn [["foo", "oof"], ["com", "moc"]]

testRewriteAttrName = rewritten == ldiff
    where
        [rewritten] = rewriteAttrs attrs [testLdif1]
        attrs = map makeRewriteAttrs [["A", "a1"]]
        ldiff = genLdif "dc=foo,dc=com"
            [ ("a1", ["A1", "A2"])
            , ("B", ["B1"])
            , ("C", ["C1"])
            ]

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

testRewriteDnAttrs = rewritten == ldiff
    where
        rewritten = rewriteAttrs attrs [testLdif1, testLdif2']
        attrs = map makeRewriteAttrs
            [ ["foo", "A", "^A1$", "a1"]
            , ["bar", "B", "B2", "hubba"]
            ]
        ldiff =
            [ genLdif "dc=foo,dc=com"
                [ ("A", ["a1", "A2"])
                , ("B", ["B1"])
                , ("C", ["C1"])
                ]
            , genLdif "dc=bar,dc=org"
                [ ("A", ["A1"])
                , ("B", ["hubba"])
                , ("D", ["D1"])
                ]
            ]

testReplaceAttrs = either (const False) (== [ldiff]) rewritten
    where
        rewritten = applyLdif rpl [testLdif1]
        rpl = [("dc=foo,dc=com",
            LDIFChange [LDAPMod LdapModReplace "B" ["hubba", "bubba"]])]
        ldiff = genLdif "dc=foo,dc=com"
            [ ("A", ["A1", "A2"])
            , ("B", ["hubba", "bubba"])
            , ("C", ["C1"])
            ]

testFilterDnId = filtered == testLdif1
    where
        [filtered] = filterDn attrs [testLdif1]
        attrs = map makeDnFilter []

testFilterDn = filtered == testLdif1
    where
        [filtered] = filterDn attrs [testLdif1, testLdif1']
        attrs = map makeDnFilter ["bar"]

testFilterAttrsId = filtered == testLdif1
    where
        [filtered] = filterEntries attrs [testLdif1]
        attrs = map makeAttrFilter []

testFilterAttrs = filtered == ldiff
    where
        [filtered] = filterEntries attrs [testLdif1]
        attrs = map makeAttrFilter [["B"]]
        ldiff = genLdif "dc=foo,dc=com"
            [ ("A", ["A1", "A2"])
            , ("C", ["C1"])
            ]

testFilterDnAttrs = filtered == ldiff
    where
        [filtered] = filterEntries attrs [testLdif1]
        attrs = map makeAttrFilter [["dc=foo", "B"]]
        ldiff = genLdif "dc=foo,dc=com"
            [ ("A", ["A1", "A2"])
            , ("C", ["C1"])
            ]

testFilterAttrVals = filtered == ldiff
    where
        [filtered] = filterEntries attrs [testLdif1]
        attrs = map makeAttrFilter [["", "A", "A2"]]
        ldiff = genLdif "dc=foo,dc=com"
            [ ("A", ["A1"])
            , ("B", ["B1"])
            , ("C", ["C1"])
            ]

testFilterLdif = filtered == ldiff
    where
        [filtered] = filterLdif attrs [testLdif1, testLdif2']
        attrs = makeDnFilter "bar" : map makeAttrFilter [["", "A", "A2"]]
        ldiff = genLdif "dc=foo,dc=com"
            [ ("A", ["A1"])
            , ("B", ["B1"])
            , ("C", ["C1"])
            ]

