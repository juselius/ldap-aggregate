-- | Update source tree

module Main where

import System.Environment
import System.IO
import qualified Data.ByteString.Char8 as BS

import LDAPRelay.DirectoryTree

main :: IO ()
main = do
    args <- getArgs
    case args of
        _ -> doS
    where
        doS = do
            ldap <- bindDIT "dc=source"
            ldif <- readLdif "./ldif/source.update.ldif"
            let l = parseLdif ldif
            print l
            runLdif ldap l
            printDIT ldap "dc=source"
        readLdif f = withFile f ReadMode BS.hGetContents

