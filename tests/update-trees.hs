-- | Update source tree

module Main where

import System.Environment
import System.IO
import LDAP
import LDIF.Simple
import qualified Data.ByteString.Char8 as BS

import TestUtils

main :: IO ()
main = do
    args <- getArgs
    case args of
        _ -> doS
    where
        doS = do
            ldap <- bindDIT "dc=source"
            ldif <- readLdif "./ldif/source.update.ldif"
            mapM_ (\(LDIFMod dn attrs) ->
                    ldapModify ldap dn [attrs]) $
                        either (error . show) (id) (parseLDIFStr "" ldif)
            printDIT ldap "dc=source"
        readLdif f = withFile f ReadMode BS.hGetContents

