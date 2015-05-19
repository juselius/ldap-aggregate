--
-- <jonas.juselius@uit.no> 2014
--
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-missing-signatures #-}

module TestUtils (
      clearSubTree
    , clearTree'
    , commitLdap
    , populateSource
    , populateTarget
) where

import System.IO
import Text.Regex.TDFA
import LDAP
import DITs
import LDIF
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- | Convert a LDIF string of LDAP search results to LDAPMod for add
ldifStrToLdapAdd :: T.Text -> LDIF
ldifStrToLdapAdd str =
    either (error . show) id $ parseLdifStr "" str

clearTree' :: T.Text -> IO ()
clearTree' tree = do
    ldap <- bindDIT $ DIT
        "ldap://localhost:389"
        ("cn=admin," `T.append` tree')
        "secret" [] [] []
    printSubTree ldap sb
    clearSubTree ldap sb
    printSubTree ldap sb
    where
        tree' = "dc=" `T.append` tree
        sb = SearchBase tree' "objectClass=*"

clearSubTree :: LDAP -> SearchBase -> IO ()
clearSubTree ldap tree = do
    ldif <- fetchSubTree ldap tree
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
    commitLdap ldap ltree
    commitLdap ldap lpops

populateTarget :: LDAP -> IO ()
populateTarget ldap = do
    ltree <- withFile "./ldif/target.tree.ldif" ReadMode T.hGetContents
    lpops <- withFile "./ldif/target.populate.ldif" ReadMode T.hGetContents
    commitLdap ldap ltree
    -- commitLdap ldap lpops

commitLdap :: LDAP -> T.Text -> IO()
commitLdap ldap x = modifyDIT ldap mods
    where
         mods = lMod . ldifToMod . ldifStrToLdapAdd $ x

