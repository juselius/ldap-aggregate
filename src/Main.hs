--
-- <jonas.juselius@uit.no> 2014
--
-- TODO:
--  * exceptions
--  * error msgs
--  * verbosity
--  * deletes
--  * automatic container filtering
--  * parallelize
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
import qualified System.Remote.Monitoring as EKG

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
        tConn    :: LDAP
      , tDit     :: DIT
      , tRules   :: LDIFRules
      , tTree    :: LDIFEntries
      , tStamp   :: T.Text
      , sConns   :: [LDAP]
      , sDits    :: [DIT]
      , sRules   :: [LDIFRules]
      , sTree    :: LDIFEntries
      , sTreeRef :: LDIFEntries
      , sStamp   :: T.Text
    }

main :: IO ()
main = do
    void $ EKG.forkServer "localhost" 8000
    a   <- cmdArgs cmdln
    cfg <- readConfig (config a)
    let
        tDit   = targetDIT cfg
        tRules = getLdifRules tDit
        sDits  = sourceDITs cfg
        sRules = map getLdifRules sDits
    tConn   <- bindDIT tDit
    sConns  <- mapM bindDIT sDits

    let world = World
            tConn tDit tRules HM.empty epoch
            sConns sDits sRules HM.empty HM.empty epoch
        updater' = updater $ updateInterval cfg

    w <- updateSourceTrees world >>= updater'
    updateLoop updater' w

    putStrLn "done." -- never reached

updateLoop :: (World -> IO World) -> World -> IO ()
updateLoop updf w = updateLoop updf w

updater :: Int -> World -> IO World
updater delay world = do
    sts <- getCurrentTimeStamp
    w <- runUpdates world
    threadDelay $ 1000000 * delay
    tts <- getCurrentTimeStamp
    return w { tStamp = tts
             , sStamp = sts
             }

runUpdates :: World -> IO World
runUpdates world = do
    w@World{..} <- updateTargetTree world >>= updateSourceTrees
    let delta = diffLDIF tTree sTree
        dels  = diffLDIF sTree sTreeRef
        dTree = either (error . show) id $ applyLdif tTree delta
    putStrLn $ "dels: " ++ show dels
    modifyDIT tConn delta
    return w { tTree = dTree }

addTS :: T.Text -> DIT -> DIT
addTS ts d@DIT{..} =
    d { searchBases = map (addModifyTimestamp ts) searchBases }

updateTargetTree :: World -> IO World
updateTargetTree w@World{..} = do
    t <- liftM (applyLdifRules tRules) $ fetchLdif tConn tDit'
    return w { tTree = HM.union t tTree }
    where
        tDit'  = addTS tStamp tDit

updateSourceTrees :: World -> IO World
updateSourceTrees w@World{..} = do
    s <- liftM (HM.unions . zipWith applyLdifRules sRules) $
            zipWithM fetchLdif sConns sDits'
    return w { sTreeRef = sTree
             , sTree = HM.union s sTree
             }
    where
        sDits' = map (addTS sStamp) sDits

fetchLdif :: LDAP -> DIT -> IO LDIFEntries
fetchLdif l DIT{..} = do
    tree <- fetchTree l searchBases
    unless (null tree) . putStrLn $
        "DIT: " ++ show searchBases ++ "\n==> " ++ show tree ++ "\n"
    return $ ldapToLdif tree

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

epoch :: T.Text
epoch = "19700101000000Z"

