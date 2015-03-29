-- | Update source tree

module Main where

import System.Environment
import System.IO
import Aggregate
import LDIF
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
    args <- getArgs
    doS
    where
        doS = do
            ldap <- bindDIT "ldap://localhost" "cn=admin,dc=source" "secret"
            ldif <- readLdif "./ldif/source.update.ldif"
            let l = parseLdif ldif
            print l
            runLdif ldap l
            printDIT ldap "dc=source"
        readLdif f = withFile f ReadMode BS.hGetContents

