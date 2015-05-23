--
-- <jonas.juselius@uit.no> 2014
--
-- TODO:
-- * verbosity
-- * parallelize
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Paths_ldap_aggregate
import System.Console.CmdArgs
import System.Posix.Signals
import Control.Monad
-- import Control.Monad.IO.Class
-- import Control.Applicative
import Control.Concurrent
import Data.Version
import Data.List
import Data.IORef
import LDAP
import LDIF
import Config
import DITs
import Editor.Edit (Rule(..))
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
        , "kill -HUP PID initiates a full sweep of all trees."
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

-- | Update soruce and target trees and start the update loop
aggregator :: World -> IO ()
aggregator world = do
    let updater' = updater $ dt world
    w <- updateTargetTree world >>= updateSourceTrees >>= updater'
    updateLoop updater' w

-- | Run updates until a full sweep is initiated
updateLoop :: (World -> IO World) -> World -> IO ()
updateLoop updf w = updf w >>= updateLoop updf

-- | Update time stamps, process updates and sleep until next update
-- updater :: Int -> World -> WriterT (Int, String) IO World
updater :: Int -> World -> IO World
updater delay world = do
    sts <- getCurrentTimeStamp
    w <- runUpdates world
    threadDelay $ inSeconds delay
    tts <- getCurrentTimeStamp
    return w { tStamp = tts
             , sStamp = sts }

-- | Sleep until next sweep, restart worker thread(s), and reschedule sweep
scheduleSweep :: Int -> IO ()
scheduleSweep i = do
    threadDelay $ inSeconds i
    raiseSignal sigHUP
    scheduleSweep i

-- | Restart the worker thread, triggering a full sweep of all DITs
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

-- | Fetch target tree LDIF, apply rules and time stamp
updateTargetTree :: World -> IO World
updateTargetTree w@World{..} = do
    l <- fetchLdif tConn tDit'
    let l' = applyLdifRules (ignoreTwigs l) l -- Ignore non-leaf entries
        t  = applyLdifRules tRules l'
    return w { tTree   = HM.union t tTree }
    where
        tDit'  = updateTimeStamp tStamp tDit

-- | Fetch source trees LDIF, apply rules, unify and time stamp
updateSourceTrees :: World -> IO World
updateSourceTrees w@World{..} = do
    l <- zipWithM fetchLdif sConns sDits'
    let l' = zipWith (applyLdifRules . ignoreTwigs) l l -- Ignore non-leaf entries
        s = (HM.unions . zipWith applyLdifRules sRules) l'
    return w { sTree = HM.union s sTree }
    where
        sDits' = map (updateTimeStamp sStamp) sDits

-- | Fetch and parse ldif data from all subtrees in a DIT
fetchLdif :: LDAP -> DIT -> IO LDIFEntries
fetchLdif l DIT{..} = do
    tree <- fetchTree l searchBases
    -- unless (null tree) . putStrLn $
        -- "DIT: " ++ show searchBases ++ "\n==> " ++ show tree ++ "\n"
    return $ ldapToLdif tree

-- | Add automatic ignore rules for all non-leaf entries
ignoreTwigs :: LDIFEntries -> LDIFRules
ignoreTwigs (HM.toList -> l) = LDIFRules [] twigR []
        where
            twigR = map (\s -> Delete (anchor s) Done) $ getTwigs l
            getTwigs x = nub $ foldl' chopLeaves [] x
            chopLeaves acc (dn, _) = twig dn : acc
            twig dn = T.tail $ T.dropWhile (/= ',') dn
            anchor s = "^" `T.append` s `T.append` "$"

-- | Computer genesis
epoch :: T.Text
epoch = "19700101000000Z"

-- | Convert microseconds to seconds
inSeconds :: Int -> Int
inSeconds = (*) 1000000

