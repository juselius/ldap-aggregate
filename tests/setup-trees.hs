-- | Test ldap conection

{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import TestUtils
import DITs

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

