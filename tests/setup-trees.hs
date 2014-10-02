-- | Test ldap conection

module Main where

import System.Environment
import TestUtils

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["source"] -> doS
        ["target"] -> doT
        _ -> doS >> doT
    where
        doS = do
            ldap <- bindDIT "ldap://localhost:389"
                "cn=admin,dc=source" "secret"
            populateSource ldap
            printDIT ldap "dc=source"
        doT = do
            ldap <- bindDIT "ldap://localhost:389"
                "cn=admin,dc=target" "secret"
            populateTarget ldap
            printDIT ldap "dc=target"

