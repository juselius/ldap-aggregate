--
-- <jonas.juselius@uit.no> 2015
--
{-# LANGUAGE OverloadedStrings #-}
module Config where

import Data.Maybe (fromJust)
import Data.Monoid
import Data.Yaml
import Control.Applicative
import Control.Monad
import qualified Data.Text as T

data Config = Config {
      targetDIT :: DIT
    , sourceDIT :: [DIT]
    } deriving (Show)

data DIT = DIT {
      ditUri :: T.Text
    , ditBaseDn :: T.Text
    , ditBindDn :: T.Text
    , ditPasswd :: T.Text
    , ditSearchBases :: [SearchBase]
    , ditIgnoreFilters :: [IgnoreFilter]
    , ditRewriteFilters :: [RewriteFilter]
    } deriving (Show)

data SearchBase = SearchBase {
      searchBase :: T.Text
    , searchFilter :: T.Text
    } deriving (Show)

data IgnoreFilter = Ignore {
        igDn :: Maybe T.Text
      , igAttr :: Maybe T.Text
      , igValue :: Maybe T.Text
    } deriving (Show)

type FromTo = (T.Text, T.Text)

data RewriteFilter = Rewrite {
        rwDn :: Maybe FromTo
      , rwAttr :: Maybe FromTo
      , rwValue :: Maybe FromTo
    } deriving (Show)

instance FromJSON Config where
    parseJSON (Object o) = Config
        <$>  (o .: "target")
        <*>  (o .: "source")
    parseJSON _ = mzero

instance FromJSON DIT where
    parseJSON (Object o) = DIT
        <$> o .: "uri"
        <*> o .: "base"
        <*> o .: "binddn"
        <*> o .: "password"
        <*> o .: "search"
        <*> o .:? "ignore" .!= mempty
        <*> o .:? "rewrite" .!= mempty
    parseJSON _ = mzero

instance FromJSON SearchBase where
    parseJSON (Object o) = SearchBase
        <$> o .: "basedn"
        <*> o .:? "filter" .!= mempty
    parseJSON _ = mzero

instance FromJSON IgnoreFilter where
    parseJSON (Object o) = Ignore
        <$> o .:? "dn" .!= mempty
        <*> o .:? "attr" .!= mempty
        <*> o .:? "value" .!= mempty
    parseJSON _ = mzero

instance FromJSON RewriteFilter where
    parseJSON (Object o) = Rewrite
        <$> o `getFromTo` "dn"
        <*> o `getFromTo` "attr"
        <*> o `getFromTo` "value"
        where
            getFromTo x s = fmap parseFromTo (x .:? s)
            parseFromTo (Just (Object x)) =
                flip parseMaybe x $ \y -> (,)
                    <$> y .: "from"
                    <*> y .: "to"
            parseFromTo _ = Nothing

    parseJSON _ = mzero

readConfig :: FilePath -> IO Config
readConfig f = liftM fromJust $ decodeFile f
