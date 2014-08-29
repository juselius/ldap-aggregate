--
-- <jonas.juselius@uit.no> 2014
--
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-missing-signatures #-}

module TestUtils (
      clearTree
    , clearTree'
    , populateSource
    , populateTarget
) where

import System.IO
import Text.Regex.Posix
import LDAP
import LDIF.Simple
import qualified Data.ByteString.Char8 as BS
import qualified Text.RegexPR as PR

import LDAPRelay.Utils

clearTree' :: String -> IO ()
clearTree' tree = do
    ldap <- bindDIT tree
    printDIT ldap tree
    clearTree ldap tree
    printDIT ldap tree

clearTree :: LDAP -> String -> IO ()
clearTree ldap tree = do
    ldif <- getDIT ldap tree
    let ldif' = reverse $ filter delEntry ldif
    mapM_ (\dn-> ldapDelete ldap (ledn dn)) ldif'
    where
        delEntry e
            | ledn e =~ ("^dc=(source|target)$" :: String) :: Bool = False
            | ledn e =~ ("^cn=admin,dc=[^=]*$" :: String) :: Bool = False
            | otherwise = True

populateSource :: LDAP -> IO ()
populateSource ldap = do
    ltree <- withFile "./ldif/source.tree.ldif" ReadMode BS.hGetContents
    lpops <- withFile "./ldif/source.populate.ldif" ReadMode BS.hGetContents
    let commit x = mapM_ (\(dn, attrs) ->
            ldapAdd ldap dn attrs) (ldata2LdapMod x)
    commit ltree
    commit lpops

populateTarget :: LDAP -> IO ()
populateTarget ldap = do
    ltree <- withFile "./ldif/target.tree.ldif" ReadMode BS.hGetContents
    lpops <- withFile "./ldif/target.populate.ldif" ReadMode BS.hGetContents
    let commit x = mapM_ (\(dn, attrs) ->
            ldapAdd ldap dn attrs) (ldata2LdapMod x)
    commit ltree
    commit lpops

--
-- Test data below
--

testDN = "uid=foobar,ou=groups,dc=source"
testRelocPatterns = [
      ("dc=source", "dc=target")
    , ("dc=foo", "dc=bar")
    ]
testLDIF = LDIFEntry Nothing $ LDAPEntry testDN [
      ("uid", ["foo"])
    , ("member", ["foo", "bar"])
    ]


