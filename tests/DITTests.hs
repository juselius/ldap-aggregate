{-# LANGUAGE OverloadedStrings #-}
module TestsDIT (
    testDITs
) where

import Test.Tasty
import Test.Tasty.HUnit
import Config
import DITs

-- import Debug.Trace

configTests :: TestTree
configTests =
    testGroup "YML Config" [
      testCase "parse config " $ testParse
      ]

testParse :: Assertion
testParse = do
    c <- readConfig "ldap-aggregate.yml"
    c @?= Config tDIT [sDIT0]

tDIT :: DIT
tDIT = DIT {
      uri = "ldaps://localhost:389"
    , binddn = "cn=admin,dc=target"
    , passwd = "secret"
    , basedn = "dc=target"
    , searchBases    = [
          SearchBase "ou=users,dc=target" ""
        , SearchBase "ou=groups,dc=target" ""
        , SearchBase "cn=user,ou=members,dc=target" ""
        ]
    , ignoreFilters  = []
    , rewriteFilters = []
    }

sDIT0 :: DIT
sDIT0 = DIT {
      uri = "ldaps://localhost:389"
    , binddn = "cn=admin,dc=source"
    , passwd = "secret"
    , basedn = "dc=source"
    , searchBases    = [
          SearchBase "ou=users,dc=source" ""
        , SearchBase "ou=groups,dc=source" ""
        , SearchBase "cn=user,ou=members,dc=source" ""
        ]
    , ignoreFilters  = []
    , rewriteFilters = []
    }
    -- ignore:
    --     - dn: ou=import,ou=users,dc=target
    --     - dn: ou=import,ou=groups,dc=target
    --     - dn: cn=vip,ou=members,dc=target
    --     - attr: entryUUID
    --     - dn: cn=Marve Fleksnes,ou=users,dc=target
    --       attr: ou
    --     - dn: cn=Marve Fleksnes,ou=users,dc=target
    --       attr: ou
    --       value: foo
