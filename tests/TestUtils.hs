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
    , bindDIT
    , printDIT
) where

import System.IO
import Text.Regex.TDFA
import LDAP
import DITs
import SimpleLDIF
import CombEditor
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- | Convert a LDIF string of LDAP search results to LDAPMod for add
ldifStrToLdapAdd :: T.Text -> [(String, [LDAPMod])]
ldifStrToLdapAdd str =
        map toMod ldif
    where
        ldif = extractEntries $ parseLdifStr "" str
        extractEntries = either (error . show) id
        toMod (dn, LDIFAdd x) = (dn, x)
        toMod (dn, _) = (dn, [])


clearTree' :: String -> IO ()
clearTree' tree = do
    ldap <- bindLdap "ldap://localhost:389" ("cn=admin," ++ tree) "secret"
    printSubTree ldap tree
    clearTree ldap tree
    printSubTree ldap tree

clearTree :: LDAP -> String -> IO ()
clearTree ldap tree = do
    ldif <- getSubTree ldap tree
    let ldif' = reverse $ filter delEntry ldif
    mapM_ (ldapDelete ldap . ledn) ldif'
    where
        delEntry e
            | ledn e =~ ("^dc=(source|target)$" :: String) :: Bool = False
            | ledn e =~ ("^cn=admin,dc=[^=]*$" :: String) :: Bool = False
            | otherwise = True

populateSource :: LDAP -> IO ()
populateSource ldap = do
    ltree <- withFile "./ldif/source.tree.ldif" ReadMode T.hGetContents
    lpops <- withFile "./ldif/source.populate.ldif" ReadMode T.hGetContents
    let commit x = mapM_ (uncurry (ldapAdd ldap)) (ldifStrToLdapAdd x)
    commit ltree
    commit lpops

populateTarget :: LDAP -> IO ()
populateTarget ldap = do
    ltree <- withFile "./ldif/target.tree.ldif" ReadMode T.hGetContents
    lpops <- withFile "./ldif/target.populate.ldif" ReadMode T.hGetContents
    let commit x = mapM_ (uncurry (ldapAdd ldap)) (ldifStrToLdapAdd x)
    commit ltree
    commit lpops

--
-- Test data
--

testDN = "uid=foobar,ou=groups,dc=source"

testRelocPatterns = [
      ("dc=source", "dc=target")
    , ("dc=foo", "dc=bar")
    ]

testLDIF = LDIFEntry $ LDAPEntry "dc=hello,dc=world" [
      ("uid", ["foo"])
    , ("member", ["foo", "bar"])
    , ("description", ["foobar"])
    ]

testRewriteAttrs = [
      ("uid", ("oo", "aa"))
    , ("member", ("foo", "oof"))
    ]

testIgnore = [
    ("uid=foo", ["description"])
    ]
