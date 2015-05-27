-- | Test ldap conection

{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO
import System.Environment
import TestUtils
import Aggregate.DITs
import LDAP
import LDIF
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["source"] -> doS
        ["target"] -> doT
        _ -> doS >> doT
    where
        doS = do
            ldap <- bindDIT $ DIT
                "ldap://localhost:389"
                "cn=admin,dc=source"
                "secret" [] [] []
            populateSource ldap
            printSubTree ldap $ SearchBase "dc=source" "objectClass=*"
        doT = do
            ldap <- bindDIT $ DIT
                "ldap://localhost:389"
                "cn=admin,dc=target"
                "secret" [] [] []
            populateTarget ldap
            printSubTree ldap $ SearchBase "dc=target" "objectClass=*"

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
    commitLdap ldap lpops

