{-# LANGUAGE OverloadedStrings #-}
module DITTests (
    ditTests
) where

import Test.Tasty
import Test.Tasty.HUnit
import Editor
import Config
import DITs

-- import Debug.Trace

ditTests :: TestTree
ditTests =
    testGroup "YML DIT" [
      testCase "parse config " $ testParse
      ]

testParse :: Assertion
testParse = do
    c <- readConfig "test.yml"
    c @?= Config tDIT [sDIT0]

tDIT :: DIT
tDIT = DIT {
      uri = "target"
    , binddn = "cn=admin,dc=target"
    , passwd = "secret"
    , basedn = "dc=target"
    , searchBases    = [
          SearchBase "dc=target" ""
        ]
    , ignoreFilters  = [
        IgnoreRule $
            Delete "ou=users,dc=target" $
              Delete "entryUUID" $
                Delete "felgen" Done
        ]
    , rewriteFilters = []
    }

sDIT0 :: DIT
sDIT0 = DIT {
      uri = "source"
    , binddn = "cn=admin,dc=source"
    , passwd = "secret"
    , basedn = "dc=source"
    , searchBases    = [
          SearchBase "dc=source" "*"
        ]
    , ignoreFilters  = [
          IgnoreRule $ Delete "ou=users,dc=source" Done
        , IgnoreRule $ Delete ".*" $ Delete "mail" Done
        , IgnoreRule $ Delete ".*" $ Delete ".*" $ Delete "^foo@.*" Done
        ]
    , rewriteFilters = [
          RewriteRule $ Subst "ou=users,dc=source" "ou=u,ou=target" Done
        , RewriteRule $ Subst "(.*)" "\\1" $ Subst "mail" "post" Done
        , RewriteRule $
            Subst "(.*)" "\\1" $
              Subst "mail" "mail" $
                Subst "^foo(@.*)" "bar\\1" Done
        ]
    }
