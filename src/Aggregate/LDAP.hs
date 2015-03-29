--
-- <jonas.juselius@uit.no> 2014
--
{-# LANGUAGE OverloadedStrings #-}
module Aggregate.LDAP (
      bindDIT
    , printSubTree
    , getSubTree
    , askBindPw
    , ldapCommit
    , LDAPEntry(..)
    , LDAPMod(..)
    , DIT(..)
    , SearchBase(..)
    ) where

import System.IO
import LDAP
import Aggregate.Alter
import SimpleLDIF
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM

data DIT = DIT {
      ditUri :: T.Text
    , ditBaseDn :: T.Text
    , ditBindDn :: T.Text
    , ditPasswd :: T.Text
    , ditSearchBases :: [SearchBase]
    , ditIgnoreFilters :: [IgnoreCriterion]
    , ditRewriteFilters :: [RewriteCriterion]
    } deriving (Show)

data SearchBase = SearchBase {
      searchBase :: T.Text
    , searchFilter :: T.Text
    } deriving (Show)

newtype IgnoreCriterion = IgnoreCriterion [Criterion Pattern] deriving (Show)
newtype RewriteCriterion = RewriteCriterion [Criterion FromTo] deriving (Show)

printSubTree :: LDAP -> String -> IO ()
printSubTree ldap tree = do
    ldif <- getSubTree ldap tree
    putStrLn . init . T.unpack $ foldl prettify  "" ldif
    putStrLn "--"
    where
        prettify s (LDAPEntry dn e) =
            showLdif l `T.append` "\n" `T.append`  s
            where
                e' = entryToText e
                dn' = T.pack dn
                l = HM.fromList [(dn', LDIFAdd dn' e')]

entryToText :: [(String, [String])] -> [(T.Text, [T.Text])]
entryToText e = map (\(dn, x) -> (T.pack dn, map T.pack x)) e

bindDIT :: DIT-> IO LDAP
bindDIT (DIT uri _ binddn pw _ _ _) = do
    ldap <- ldapInitialize uri
    ldapSimpleBind ldap binddn pw
    return ldap

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

ldapCommit :: LDAP -> LDIF -> IO ()
ldapCommit ldap =
    mapM_ runMod
    where
        runMod (dn, entry) = case entry of
            LDIFAdd _ e -> ldapAdd ldap dn e
            LDIFChange _ e -> ldapModify ldap dn e
            LDIFDelete _ -> ldapDelete ldap dn

