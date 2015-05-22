--
-- <jonas.juselius@uit.no> 2015
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf #-}
module Config (
      Config(..)
    , readConfig
    , logVerbose
) where

import Data.Yaml
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Writer.Lazy
import DITs

data Config = Config {
        updateInterval :: Int
      , sweepInterval  :: Int
      , targetDIT      :: DIT
      , sourceDITs     :: [DIT]
    } deriving (Show, Eq)

instance FromJSON Config where
    parseJSON (Object o) = Config
        <$>  o .:? "updateInterval" .!= 60
        <*>  o .:? "sweepInterval" .!= 86400
        <*>  o .: "target"
        <*>  o .: "sources"
    parseJSON _ = mzero

readConfig :: FilePath -> IO Config
readConfig f = liftM (either (error . show) id) $ decodeFileEither f

logVerbose :: Monad m => Int -> String -> WriterT [(Int, String)] m ()
logVerbose lvl msg = writer ((),[(lvl, msg)])
