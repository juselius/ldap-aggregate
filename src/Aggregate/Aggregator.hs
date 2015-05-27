{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Aggregate.Aggregator (
      aggregator
    , inSeconds
    , World(..)
    ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer.Strict
import LDAP
import LDIF
import Aggregate.DITs
import LDIF.Editor.Rules (Rule(..))
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
-- import qualified Data.ByteString as BS

data World = World {
        dt        :: Int
      , dbglvl    :: Int
      , tConn     :: LDAP
      , tDit      :: DIT
      , tRules    :: LDIFRules
      , tTree     :: LDIFEntries
      , tStamp    :: T.Text
      , sConns    :: [LDAP]
      , sDits     :: [DIT]
      , sRules    :: [LDIFRules]
      , sTree     :: LDIFEntries
      , sStamp    :: T.Text
    }

-- | Update soruce and target trees and start the update loop
aggregator :: World -> IO ()
aggregator world = do
    let updater' = updater (dbglvl world) (dt world)
    updater' world  >>= updateLoop updater'

-- | Run updates until a full sweep is initiated
updateLoop :: (World -> IO World) -> World -> IO ()
updateLoop updf w = updf w >>= updateLoop updf

-- | Update time stamps, process updates and sleep until next update
updater :: Int -> Int -> World -> IO World
updater lvl delay world = do
    sts <- getCurrentTimeStamp
    (w, l) <- runWriterT $ runUpdates world
    printLog lvl l
    threadDelay $ inSeconds delay
    tts <- getCurrentTimeStamp
    return w { tStamp = tts
             , sStamp = sts }

-- | Update and apply rules to the source and target trees, diff and commit
-- changes.
runUpdates :: World -> Log IO World
runUpdates world = do
    w@World{..} <- updateTargetTree world >>= updateSourceTrees
    let delta = diffLDIF tTree sTree
        dTree = either (error . show) id $ applyLdif tTree delta
    modifyDIT tConn delta
    return w { tTree = dTree }

-- | Fetch target tree LDIF, apply rules and time stamp
updateTargetTree :: World -> Log IO World
updateTargetTree w@World{..} = do
    l <- fetchLdif tConn tDit'
    let t = applyLdifRules tRules l
    return w { tTree = HM.union t tTree }
    where
        tDit'  = updateTimeStamp tStamp tDit

-- | Fetch source trees LDIF, apply rules, unify and time stamp
updateSourceTrees :: World -> Log IO World
updateSourceTrees w@World{..} = do
    l <- zipWithM fetchLdif sConns sDits'
    let s = (HM.unions . zipWith applyLdifRules sRules) l
    return w { sTree = HM.union s sTree }
    where
        sDits' = map (updateTimeStamp sStamp) sDits

-- | Fetch and parse ldif data from all subtrees in a DIT
fetchLdif :: LDAP -> DIT -> Log IO LDIFEntries
fetchLdif l DIT{..} = do
    tree <- liftIO $ fetchTree l searchBases
    unless (null tree) . logDbg 3 $
        "DIT: " ++ show searchBases ++ "\n==> " ++ show tree ++ "\n"
    return $ ldapToLdif tree

-- | Add automatic ignore rules for all non-leaf entries
-- This is slow, parallelizes poorly and generally not a good idea.
ignoreTwigs :: LDIFEntries -> LDIFRules
ignoreTwigs l = LDIFRules [] (HS.toList twigs) []
    where
        twigs = HM.foldlWithKey' chopLeaves HS.empty l
        chopLeaves acc dn _ = HS.insert (twig dn `Delete` Done) acc
        twig dn = anchor . T.tail $ T.dropWhile (/= ',') dn
        anchor s = "^" `T.append` s `T.append` "$"

-- | Given a verbosity level, print all log entries with a smaller level
printLog :: Int -> [(Int, String)] -> IO ()
printLog lvl = mapM_ prlog
    where
        prlog (n, s) = when (n <= lvl) $ putStrLn s

-- | Convert microseconds to seconds
inSeconds :: Int -> Int
inSeconds = (*) 1000000

