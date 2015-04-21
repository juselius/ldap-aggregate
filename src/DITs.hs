--
-- <jonas.juselius@uit.no> 2014
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module DITs (
      bindLdap
    , printSubTree
    , getSubTree
    , askBindPw
    , commitLdap
    , applyLdifRules
    , getLdifRules
    , LDAPEntry(..)
    , LDAPMod(..)
    , DIT(..)
    , SearchBase(..)
    , LDIFRules(..)
    ) where

import System.IO
import Data.Yaml
import Data.Monoid
import Control.Applicative
import Control.Monad
import LDAP
import LDIF
import Editor
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM

data DIT = DIT {
      uri :: T.Text
    , binddn :: T.Text
    , passwd :: T.Text
    , basedn :: T.Text
    , searchBases    :: [SearchBase]
    , ignoreFilters  :: [IgnoreRule]
    , rewriteFilters :: [RewriteRule]
    } deriving (Show, Eq)

data SearchBase = SearchBase {
      searchBase :: T.Text
    , searchFilter :: T.Text
    } deriving (Show, Eq, Ord)

data LDIFRules = LDIFRules {
      rewriteRules :: [Rule T.Text]
    , ignoreRules  :: [Rule T.Text]
    , insertRules  :: [Rule T.Text]
    }

instance FromJSON DIT where
    parseJSON (Object o) = fmap addAttrRewriteDn $ DIT
        <$> o .: "uri"
        <*> o .: "binddn"
        <*> o .: "password"
        <*> o .: "base"
        <*> o .: "search"
        <*> o .:? "ignore" .!= mempty
        <*> o .:? "rewrite" .!= mempty
    parseJSON _ = mzero

instance FromJSON SearchBase where
    parseJSON (Object o) = SearchBase
        <$> o .: "basedn"
        <*> o .:? "filter" .!= mempty
    parseJSON _ = mzero

getLdifRules :: DIT -> LDIFRules
getLdifRules DIT{..} = LDIFRules rw ign ins
    where
        rw  = map doRewrite rewriteFilters
        ign = map doIgnore  ignoreFilters
        ins = []

applyLdifRules :: LDIFRules -> LDIFEntries -> LDIFEntries
applyLdifRules LDIFRules{..} =
      reconcile
    . runEdits insertRules
    . runEdits rewriteRules
    . runEdits ignoreRules
    where
        reconcile = HM.foldl' (\acc v -> HM.insert (rDn v) v acc) mempty

-- for every dn rewrite, add the corresponding rewrite to attr dn
addAttrRewriteDn :: DIT -> DIT
addAttrRewriteDn d@DIT{..} =
    d { rewriteFilters = foldl df mempty rewriteFilters }
    where
        df acc r@(RewriteRule (Subst f t _)) =
            RewriteRule (Cont ".*" (Subst f t Done)):r:acc
        df acc r = r:acc

bindLdap :: DIT -> IO LDAP
bindLdap DIT{..} = do
    ldap <- ldapInitialize uri'
    ldapSimpleBind ldap binddn' passwd'
    return ldap
    where
        uri' = T.unpack uri
        binddn' = T.unpack binddn
        passwd' = T.unpack passwd

getSubTree :: LDAP -> String -> IO [LDAPEntry]
getSubTree ldap tree =
    ldapSearch ldap (Just tree) LdapScopeSubtree Nothing LDAPAllUserAttrs False

askBindPw :: IO String
askBindPw = do
    putStr "bindpw: "
    hFlush stdout
    hSetEcho stdout False
    pw <- getLine
    hSetEcho stdout True
    putStrLn ""
    return pw

commitLdap :: LDAP -> LDIFMods -> IO ()
commitLdap ldap ldif =
    mapM_ runMod $ HM.toList ldif
    where
        runMod (T.unpack -> dn, entry) = case entry of
            LDIFAdd    _ (recordToLdapAdd -> e) -> ldapAdd ldap dn e
            LDIFChange _ (recordToLdapMod -> e) -> ldapModify ldap dn e
            LDIFDelete _                        -> ldapDelete ldap dn

printSubTree :: LDAP -> String -> IO ()
printSubTree ldap tree = do
    ldif <- getSubTree ldap tree
    print $ ldapToLdif ldif

