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
    , LDAPEntry(..)
    , LDAPMod(..)
    , DIT(..)
    , SearchBase(..)
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
    } deriving (Show, Eq)

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

-- for every dn rewrite, add the corresponding rewrite to attr dn
addAttrRewriteDn :: DIT -> DIT
addAttrRewriteDn d@DIT{..} =
    d { rewriteFilters = foldl df mempty rewriteFilters }
    where
        df acc r@(RewriteRule (Subst f t _))
            | "(.*)" <- f = r:acc
            | otherwise = RewriteRule (Subst f t (Subst f t Done)):r:acc
        df acc _ = acc

instance FromJSON SearchBase where
    parseJSON (Object o) = SearchBase
        <$> o .: "basedn"
        <*> o .:? "filter" .!= mempty
    parseJSON _ = mzero

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

commitLdap :: LDAP -> LDIF -> IO ()
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
    putStrLn . T.unpack . showLdif $ ldapToLdif ldif

