--
-- <jonas.juselius@uit.no> 2014
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Paths_ldap_aggregate
import System.Console.CmdArgs
import Control.Monad
import LDAP
import SimpleLDIF
import Config
import Aggregate
import Data.Version

data CmdLine = CmdLine {
      inpfile :: FilePath
    } deriving (Data, Typeable, Show, Eq)

cmdln :: CmdLine
cmdln = CmdLine {
    inpfile = def
        &= opt ("ldap-aggregate.yml" :: FilePath)
        &= argPos 0
        &= typFile
    }
    &= verbosity
    &= help "Aggregate, filter and rewrite LDAP trees."
    &= summary ("Version "
        ++ showVersion version
        ++ ", (c) Jonas Juselius 2015"
        )
    &= details [
          "LDAP aggregate"
        , ""
        , "Aggregate, filter and rewrite LDIF from one or more source DITs"
        , "onto a target DIT."
        , ""
        ]

main :: IO ()
main = do
    a <- cmdArgs cmdln
    config <- readConfig (inpfile a)
    ldap <- bindLdap $ targetDIT config
    tTree <- runDIT $ targetDIT config
    forM_ (sourceDIT config) $ \s -> do
        sTree <- runDIT s
        updateDIT ldap sTree tTree
    putStrLn "done."

runDIT :: DIT -> IO LDIF
runDIT dit = getLdif dit >>= applyEntryFilters (genEntryFilters dit)

updateDIT :: LDAP -> LDIF -> LDIF -> IO ()
updateDIT ldap s t = commitLdif ldap $ diffLDIF t s

getLdif :: DIT -> IO [LDIF]
getLdif dit = do
    l <- bindLdap dit
    getDIT l (ditBaseDn dit) >>= mapM ldapEntryToLDIF

bindLdap :: DIT -> IO LDAP
bindLdap dit = bindDIT (ditUri dit) (ditBindDn dit) (ditPasswd dit)

genEntryFilters = undefined
applyEntryFilters = undefined
commitLdif = undefined
ldapEntryToLDIF = undefined
bindDIT = undefined
getDIT = undefined
