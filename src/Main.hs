--
-- <jonas.juselius@uit.no> 2014
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Paths_ldap_aggregate
import System.Console.CmdArgs
import System.Locale
import Control.Monad
import Control.Concurrent (threadDelay)
import LDAP
import LDIF
import Config
import DITs
import Data.Version
import Data.Time.Clock
import Data.Time.Format
import qualified Data.Text as T

data CmdLine = CmdLine {
      config :: FilePath
    } deriving (Data, Typeable, Show, Eq)

cmdln :: CmdLine
cmdln = CmdLine {
    config = def
        &= opt ("ldap-aggregate.yml" :: FilePath)
        &= argPos 0
        &= typFile
    }
    &= verbosity
    &= help "Edit, filter and rewrite LDAP trees."
    &= summary ("Version "
        ++ showVersion version
        ++ ", (c) Jonas Juselius 2015"
        )
    &= details [
          "LDAP aggregate"
        , ""
        , "Edit, filter and rewrite LDIF from one or more source DITs"
        , "onto a target DIT."
        , ""
        ]

data World = World {
      tConn  :: LDAP
    , tDit   :: DIT
    , tRules :: LDIFRules
    , sConns :: [LDAP]
    , sDits  :: [DIT]
    , sRules :: [LDIFRules]
    }

main :: IO ()
main = do
    a   <- cmdArgs cmdln
    cfg <- readConfig (config a)

    let
        tDit   = targetDIT cfg
        tRules = getLdifRules tDit
        sDits  = sourceDITs cfg
        sRules = map getLdifRules sDits
    tConn   <- bindDIT tDit
    sConns  <- mapM bindDIT sDits

    let world = World tConn tDit tRules sConns sDits sRules

    runUpdates genesis world

    void $ forever $ do
        ts <- getCurrentTimeStamp
        runUpdates ts world
        threadDelay $ 10000 * updateInterval cfg

    putStrLn "done." -- never reached


genesis :: T.Text
genesis = "*"

runUpdates :: T.Text -> World -> IO ()
runUpdates ts World{..} = do
    tTree   <- liftM (applyLdifRules tRules) $ fetchLdif tConn tDit'
    sTrees  <- liftM (zipWith applyLdifRules sRules) $
        zipWithM fetchLdif sConns sDits'
    mapM_ (modifyDIT tConn) (diffTrees tTree sTrees)
    where
        tDit'  = addts tDit
        sDits' = map addts sDits
        addts d@DIT{..} =
            d { searchBases = map (addModifyTimestamp ts) searchBases }


diffTrees :: LDIFEntries -> [LDIFEntries] -> [LDIFMods]
diffTrees t = map (diffLDIF t)

updateDIT :: LDAP -> LDIFEntries -> LDIFEntries -> IO ()
updateDIT ldap s t = modifyDIT ldap $ diffLDIF t s

fetchLdif :: LDAP -> DIT -> IO LDIFEntries
fetchLdif l DIT{..} = do
    stree <- fetchTree l searchBases
    return $ ldapToLdif stree

addModifyTimestamp :: T.Text -> SearchBase -> SearchBase
addModifyTimestamp ts b@SearchBase{..} = b {
        searchFilter = addTSFilter
        }
    where
        addTSFilter =
            "(&(modifyTimestamp>=" `T.append` ts `T.append` ")"
            `T.append` sf `T.append` ")"
        sf =
            if T.head searchFilter == '('
                then searchFilter
                else "(" `T.append` searchFilter `T.append` ")"

getCurrentTimeStamp :: IO T.Text
getCurrentTimeStamp = fmap tsfmt $ getCurrentTime
    where
        tsfmt = T.pack . formatTime defaultTimeLocale "%Y%m%d%H%M%SZ"
