-- | Update source tree
{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import System.IO
import SimpleLDIF
import DITs
import qualified Data.Text.IO as T

main :: IO ()
main = do
    args <- getArgs
    doS
    where
        doS = do
            ldap <- bindLdap dit
            ldif <- readLdif "./ldif/source.update.ldif"
            let l = parseLdif ldif
            print l
            commitLdap ldap l
            printSubTree ldap "dc=source"
        readLdif f = withFile f ReadMode T.hGetContents
        dit = DIT
            "ldap://localhost"
            "cn=admin,dc=source"
            "secret"
            ""
            []
            []
            []

