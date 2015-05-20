--
-- <jonas.juselius@uit.no> 2014
--
-- TODO:
--  * exceptions
--  * error msgs
--  * verbosity
--  * deletes (auditlog?)
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
      wConf  :: Config
    , tConn  :: LDAP
    , tDit   :: DIT
    , tRules :: LDIFRules
    , sConns :: [LDAP]
    , sDits  :: [DIT]
    , sRules :: [LDIFRules]
    , sTree  :: LDIFEntries
    , tTree  :: LDIFEntries
    , tStamp :: T.Text
    , sStamp :: T.Text
    }

main :: IO ()
main = EKG.forkServer "localhost" 8000 >> do
    a   <- cmdArgs cmdln
    cfg <- readConfig (config a)

    let
        tDit   = targetDIT cfg
        tRules = getLdifRules tDit
        sDits  = sourceDITs cfg
        sRules = map getLdifRules sDits
    tConn   <- bindDIT tDit
    sConns  <- mapM bindDIT sDits

    let world = World cfg tConn tDit tRules sConns sDits sRules HM.empty HM.empty epoch epoch

    w <- runUpdates world
    threadDelay $ 1000000 * updateInterval (wConf w)
    loop w

    putStrLn "done." -- never reached

loop :: World -> IO ()
loop w@World{..} = do
    w' <- runUpdates w
    threadDelay $ 1000000 * updateInterval wConf
    ts <- getCurrentTimeStamp
    loop w' { tStamp = ts }

epoch :: T.Text
epoch = "19700101000000Z"

runUpdates :: World -> IO World
runUpdates w@World{..} = do
    ts <- getCurrentTimeStamp
    t <- liftM (applyLdifRules tRules) $ fetchLdif tConn tDit'
    s <- liftM (HM.unions . zipWith applyLdifRules sRules) $
            zipWithM fetchLdif sConns sDits'
    let sTree' = HM.union s sTree
        tTree' = HM.union t tTree
        delta = diffLDIF tTree' sTree'
        dTree = either (error . show) id $ applyLdif tTree' delta
    -- putStrLn $ "runUpdates: " ++ show delta ++ "\n--\n"
    modifyDIT tConn delta
    -- putStrLn "@@@"
    return w { sTree = sTree'
             , tTree = dTree
             , sStamp = ts
             -- , tStamp = tts
             }
    where
        tDit'  = addts tStamp tDit
        sDits' = map (addts sStamp) sDits
        addts ts d@DIT{..} =
            d { searchBases = map (addModifyTimestamp ts) searchBases }

updateDIT :: LDAP -> LDIFEntries -> LDIFEntries -> IO ()
updateDIT ldap s t = modifyDIT ldap $ diffLDIF t s

fetchLdif :: LDAP -> DIT -> IO LDIFEntries
fetchLdif l DIT{..} = do
    tree <- fetchTree l searchBases
    -- unless (null tree) . putStrLn $
    --     "DIT: " ++ show searchBases ++ "\n==> " ++ show tree ++ "\n"
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
