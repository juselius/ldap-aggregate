--
-- <jonas.juselius@uit.no> 2014
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Aggregate
import Data.IORef
import Data.Version
import Control.Monad
import Control.Concurrent
import Paths_ldap_aggregate
import System.Console.CmdArgs
import System.Posix.Signals
import qualified Data.Text as T
import qualified System.Remote.Monitoring as EKG
import qualified Data.HashMap.Lazy as HM

data CmdLine = CmdLine {
      config :: FilePath
    , debug  :: Int
    } deriving (Data, Typeable, Show, Eq)

cmdln :: CmdLine
cmdln = CmdLine {
      config = def
        &= opt ("ldap-aggregate.yml" :: FilePath)
        &= argPos 0
        &= typFile
    , debug = 0 &= typ "LEVEL" &= help "Debug level"
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

main :: IO ()
main = do
    void $ EKG.forkServer "localhost" 8000
    a <- cmdArgs cmdln
    cfg <- readConfig $ config a
    let
        tDit'   = targetDIT cfg
        tRules' = getLdifRules tDit'
        sDits'  = sourceDITs cfg
        sRules' = map getLdifRules sDits'
    tConn'   <- bindDIT tDit'
    sConns'  <- mapM bindDIT sDits'

    vrb <- getVerbosity
    let vlevel = case vrb of
            Loud   -> debug a + 1
            Normal -> debug a
            Quiet  -> -1

    let world = World (updateInterval cfg) vlevel
            tConn' tDit' tRules' HM.empty epoch
            sConns' sDits' sRules' HM.empty epoch
    aggregator world
    t <- forkIO $ aggregator world
    tid <- newIORef t
    void $ installHandler sigHUP (Catch (hupHandler tid world)) Nothing
    scheduleSweep (sweepInterval cfg)
    putStrLn "done." -- never reached

-- | Restart the worker thread, triggering a full sweep of all DITs
hupHandler :: IORef ThreadId -> World -> IO ()
hupHandler tid world = do
    t <- readIORef tid
    killThread t
    newt <- forkIO $ aggregator world
    writeIORef tid newt

-- | Sleep until next sweep, restart worker thread(s), and reschedule sweep
scheduleSweep :: Int -> IO ()
scheduleSweep i = do
    threadDelay $ inSeconds i
    raiseSignal sigHUP
    scheduleSweep i

-- | Computer genesis
epoch :: T.Text
epoch = "19700101000000Z"

