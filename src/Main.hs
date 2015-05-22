--
-- <jonas.juselius@uit.no> 2014
--
-- TODO:
--  * verbosity
--  * automatic container filtering
--  * parallelize
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Paths_ldap_aggregate
import System.Console.CmdArgs
import System.Locale
import System.Posix.Signals
import Control.Monad
import Control.Applicative
import Control.Concurrent
import LDAP
import LDIF
import Config
import DITs
import Data.Version
import Data.IORef
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
        , "kill -HUP PID initiates a full resweep of all trees."
        ]

data World = World {
        dt       :: Int
      , tConn    :: LDAP
      , tDit     :: DIT
      , tRules   :: LDIFRules
      , tTree    :: LDIFEntries
      , tStamp   :: T.Text
      , sConns   :: [LDAP]
      , sDits    :: [DIT]
      , sRules   :: [LDIFRules]
      , sTree    :: LDIFEntries
      , sStamp   :: T.Text
    }

main :: IO ()
main = do
    void $ EKG.forkServer "localhost" 8000
    a <- cmdArgs cmdln
    cfg <- readConfig $ config a
    let
        tDit   = targetDIT cfg
        tRules = getLdifRules tDit
        sDits  = sourceDITs cfg
        sRules = map getLdifRules sDits
    tConn   <- bindDIT tDit
    sConns  <- mapM bindDIT sDits

    let world = World (updateInterval cfg)
            tConn tDit tRules HM.empty epoch
            sConns sDits sRules HM.empty epoch
    t <- forkIO $ aggregator world
    tid <- newIORef t
    void $ installHandler sigHUP (Catch (hupHandler tid world)) Nothing
    scheduleSweep (sweepInterval cfg)
    putStrLn "done." -- never reached

aggregator :: World -> IO ()
aggregator world = do
    let updater' = updater $ dt world
    w <- updateSourceTrees world >>= updater'
    updateLoop updater' w

-- | Run updates until a full sweep is initiated
updateLoop :: (World -> IO World) -> World -> IO ()
updateLoop updf w = updf w >>= updateLoop updf

-- | Update time stamps, process updates and sleep until next update
updater :: Int -> World -> IO World
updater delay world = do
    sts <- getCurrentTimeStamp
    w <- runUpdates world
    threadDelay $ inSeconds delay
    tts <- getCurrentTimeStamp
    return w { tStamp = tts
             , sStamp = sts
             }


scheduleSweep :: Int -> IO ()
scheduleSweep i = do
    threadDelay $ inSeconds i
    raiseSignal sigHUP
    scheduleSweep i

hupHandler :: IORef ThreadId -> World -> IO ()
hupHandler tid world = do
    t <- readIORef tid
    killThread t
    newt <- forkIO $ aggregator world
    writeIORef tid newt

-- | Update and apply rules to the source and target trees, diff and commit
-- changes.
runUpdates :: World -> IO World
runUpdates world = do
    w@World{..} <- updateTargetTree world >>= updateSourceTrees
    let delta = diffLDIF tTree sTree
        dTree = either (error . show) id $ applyLdif tTree delta
    modifyDIT tConn delta
    return w { tTree = dTree }

-- | Add new timestamp to all search bases
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
    return w { sTree = HM.union s sTree }
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

inSeconds :: Int -> Int
inSeconds = (*) 1000000
