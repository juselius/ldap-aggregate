{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module DITTests (
    ditTests
) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.List
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
    c <- readConfig "tests/dit.yml"
    (sortConf c) @?= (sortConf $ Config 60 tDIT [sDIT0])

sortConf :: Config -> Config
sortConf c@Config{..} = c {
            targetDIT = srtDIT targetDIT
          , sourceDIT = map srtDIT sourceDIT
          }
    where
        srtDIT d@DIT{..} = d {
              searchBases = sort searchBases
            , ignoreFilters = sort ignoreFilters
            , rewriteFilters = sort rewriteFilters
            }

tDIT :: DIT
tDIT = DIT {
      uri = "target"
    , binddn = "cn=admin,dc=target"
    , passwd = "secret"
    , searchBases    = [
          SearchBase "dc=target" "*"
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
    , searchBases    = [
          SearchBase "dc=source" "*"
        ]
    , ignoreFilters  = [
          IgnoreRule $ Delete "ou=users,dc=source" Done
        , IgnoreRule $ Cont ".*" $ Delete "mail" Done
        , IgnoreRule $ Cont ".*" $ Cont ".*" $ Delete "^foo@.*" Done
        ]
    , rewriteFilters = [
          RewriteRule $ Subst "ou=users,dc=source" "ou=u,dc=target" Done
        , RewriteRule $ Cont ".*" $
            Subst "ou=users,dc=source" "ou=u,dc=target" Done
        , RewriteRule $ Cont ".*" $
            Subst "mail" "post" Done
        , RewriteRule $
            Cont ".*" $
              Cont "mail" $
                Subst "^foo(@.*)" "bar\\1" Done
        ]
    }
