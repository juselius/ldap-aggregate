--
-- <jonas.juselius@uit.no> 2014
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Aggregate.LDAP (
      bindLdap
    , printSubTree
    , getSubTree
    , askBindPw
    , commitLdap
    , LDAPEntry(..)
    , LDAPMod(..)
    , DIT(..)
    , SearchBase(..)
    , IgnoreCriterion(..)
    , RewriteCriterion(..)
    ) where

import System.IO
import LDAP
import Aggregate.Edit
import SimpleLDIF
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM

data DIT = DIT {
      uri :: T.Text
    , binddn :: T.Text
    , passwd :: T.Text
    , basedn :: T.Text
    , searchBases :: [SearchBase]
    , ignoreFilters :: [IgnoreCriterion]
    , rewriteFilters :: [RewriteCriterion]
    } deriving (Show)

data SearchBase = SearchBase {
      searchBase :: T.Text
    , searchFilter :: T.Text
    } deriving (Show)

newtype IgnoreCriterion = IgnoreCriterion [Criterion Pattern] deriving (Show)
newtype RewriteCriterion = RewriteCriterion [Criterion FromTo] deriving (Show)

bindLdap :: DIT-> IO LDAP
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
        runMod ((T.unpack -> dn), entry) = case entry of
            LDIFAdd    _ (recordToLdapAdd -> e) -> ldapAdd ldap dn e
            LDIFChange _ (recordToLdapMod -> e) -> ldapModify ldap dn e
            LDIFDelete _                        -> ldapDelete ldap dn

printSubTree :: LDAP -> String -> IO ()
printSubTree ldap tree = do
    ldif <- getSubTree ldap tree
    putStrLn . T.unpack . showLdif $ ldapToLdif ldif

