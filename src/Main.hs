--
-- <jonas.juselius@uit.no> 2014
--
-- TODO:
--  * exceptions
--  * error msgs
--  * verbosity
--  * deletes (auditlog?)
--  * automatic container filtering
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Paths_ldap_aggregate
import System.Console.CmdArgs
import System.Locale
import Control.Monad
import Control.Applicative
import Control.Concurrent (threadDelay)
import LDAP
import LDIF
import Config
import DITs
import Data.Version
import Data.Time.Clock
import Data.Time.Format
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM

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
    &= summary ("ldap-aggregate v. "
        ++ showVersion version
        )
    &= details [
          "ldap-aggregate, (c) Jonas Juselius, UiT, 2015"
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
    , sTree  :: LDIFEntries
    , tTree  :: LDIFEntries
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

    let world = World tConn tDit tRules sConns sDits sRules HM.empty HM.empty

    w <- runUpdates epoch world
    loop cfg w

    putStrLn "done." -- never reached

loop :: Config -> World -> IO ()
loop cfg w = do
    ts <- getCurrentTimeStamp
    threadDelay $ 1000000 * updateInterval cfg
    w' <- runUpdates ts w
    loop cfg w'


epoch :: T.Text
epoch = "19700101000000Z"

runUpdates :: T.Text -> World -> IO World
runUpdates ts w@World{..} = do
    t <- liftM (applyLdifRules tRules) $ fetchLdif tConn tDit'
    s <- liftM (HM.unions . zipWith applyLdifRules sRules) $
        zipWithM fetchLdif sConns sDits'
    let sTree' = HM.union sTree s
        tTree' = HM.union tTree t
    modifyDIT tConn $ diffLDIF tTree' sTree'
    return w { sTree = sTree'
             , tTree = tTree'
             }
    where
        tDit'  = addts tDit
        sDits' = map addts sDits
        addts d@DIT{..} =
            d { searchBases = map (addModifyTimestamp ts) searchBases }


diffTrees :: LDIFEntries -> [LDIFEntries] -> [LDIFMods]
diffTrees s t = map (\x -> diffLDIF s x) t

updateDIT :: LDAP -> LDIFEntries -> LDIFEntries -> IO ()
updateDIT ldap s t = modifyDIT ldap $ diffLDIF t s

fetchLdif :: LDAP -> DIT -> IO LDIFEntries
fetchLdif l DIT{..} = do
    stree <- fetchTree l searchBases
    -- print "@@@"
    -- print stree
    -- print "###"
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
getCurrentTimeStamp = tsfmt <$> getCurrentTime
    where
        tsfmt = T.pack . formatTime defaultTimeLocale "%Y%m%d%H%M%SZ"
