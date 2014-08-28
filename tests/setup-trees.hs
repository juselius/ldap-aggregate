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
            source <- bindDIT "dc=source"
            populateSource source
            printDIT source "dc=source"
        doT = do
            target <- bindDIT "dc=target"
            populateTarget target
            printDIT target "dc=target"

