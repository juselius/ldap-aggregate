--
-- <jonas.juselius@uit.no> 2014
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Aggregate.Criterion (
      IgnoreCriterion(..)
    , RewriteCriterion(..)
    ) where

import Data.Yaml
import Data.Maybe
import Data.Monoid
import Control.Applicative
import Control.Monad
import Aggregate.Edit

newtype IgnoreCriterion = IgnoreCriterion [Criterion Pattern] deriving (Show)
newtype RewriteCriterion = RewriteCriterion [Criterion FromTo] deriving (Show)

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
            noMatch = ("^$", "")
    parseJSON _ = mzero


