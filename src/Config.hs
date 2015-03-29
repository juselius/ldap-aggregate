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
import Data.Monoid
import Data.Yaml
import Control.Applicative
import Control.Monad
import LDAPAggregate.LDAP
import qualified Data.Text as T

data Config = Config {
      targetDIT :: DIT
    , sourceDIT :: [DIT]
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

instance FromJSON IgnoreCriterion where
    parseJSON (Object o) = do
        dn    <- o .:? "dn"
        attr  <- o .:? "attr"
        value <- o .:? "value"
        return $ IgnoreCriterion [
              toCriterion (attr `mplus` value) dn
            , toCriterion value attr
            , toCriterion Nothing value
            ]
        where
            toCriterion :: Maybe a -> Maybe Pattern -> Criterion Pattern
            toCriterion p v
                | isJust p = Cont v'
                | otherwise = Break v'
                where v' = fromMaybe mempty v
    parseJSON _ = mzero

noMatch :: (T.Text, T.Text)
noMatch = ("^$", "")

instance FromJSON RewriteCriterion where
    parseJSON (Object o) = do
        dn    <- o `getFromTo` "dn"
        attr  <- o `getFromTo` "attr"
        value <- o `getFromTo` "value"
        return $ RewriteCriterion [
              toCriterion (attr `mplus` value) dn
            , toCriterion value attr
            , toCriterion Nothing value
            ]
        where
            getFromTo x s = fmap parseFromTo (x .:? s)
            toCriterion :: Maybe a -> Maybe FromTo -> Criterion FromTo
            toCriterion p v
                | isJust p = Cont v'
                | otherwise = Break v'
                where v' = fromMaybe noMatch v
            parseFromTo :: Maybe Value -> Maybe FromTo
            parseFromTo (Just (Object x)) =
                flip parseMaybe x $ \y -> (,)
                    <$> y .: "from"
                    <*> y .: "to"
            parseFromTo _ = Nothing

    parseJSON _ = mzero

readConfig :: FilePath -> IO Config
readConfig f = liftM fromJust $ decodeFile f

