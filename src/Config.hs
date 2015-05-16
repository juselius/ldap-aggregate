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
) where

import Data.Maybe
import Data.Yaml
import Control.Applicative
import Control.Monad
import DITs

data Config = Config {
      updateInterval :: Int
    , targetDIT  :: DIT
    , sourceDITs :: [DIT]
    } deriving (Show, Eq)

instance FromJSON Config where
    parseJSON (Object o) = Config
        <$>  o .:? "updateInterval" .!= 60
        <*>  o .: "target"
        <*>  o .: "sources"
    parseJSON _ = mzero

readConfig :: FilePath -> IO Config
readConfig f = liftM fromJust $ decodeFile f

