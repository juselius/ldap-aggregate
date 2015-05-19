-- | Update source tree
{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import System.IO
import LDIF
import DITs
import TestUtils
import qualified Data.Text.IO as T

main :: IO ()
main = do
    args <- getArgs
    doS
    where
        doS = do
            ldap <- bindDIT dit
            ldif <- readLdif "./ldif/source.update.ldif"
            let l = parseLdif ldif
            print l
            commitLdap ldap ldif
            printSubTree ldap $ SearchBase "dc=source" "objectClass=*"
        readLdif f = withFile f ReadMode T.hGetContents
        dit = DIT
            "ldap://localhost"
            "cn=admin,dc=source"
            "secret" [] [] []

