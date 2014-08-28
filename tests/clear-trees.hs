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
        doS = clearTree' "dc=source"
        doT = clearTree' "dc=target"

