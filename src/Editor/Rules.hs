--
-- <jonas.juselius@uit.no> 2014
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Editor.Rules (
      Rule(..)
    , IgnoreRule(..)
    , RewriteRule(..)
    , InsertRule(..)
    ) where

import Data.Yaml
import Data.Maybe
import Data.Monoid
import Control.Monad
import Editor.Edit
import qualified Data.Text as T

newtype IgnoreRule  = IgnoreRule  (Rule T.Text) deriving (Show, Eq, Ord)
newtype RewriteRule = RewriteRule (Rule T.Text) deriving (Show, Eq, Ord)
newtype InsertRule  = InsertRule  (Rule T.Text) deriving (Show, Eq, Ord)

instance FromJSON IgnoreRule where
    parseJSON (Object o) = do
        dn    <- o .:? "dn"
        attr  <- o .:? "attr"
        val   <- o .:? "value"
        let
            dn'   = mkRule (attr `mplus` val) dn
            attr' = mkRule val attr
            val'  = mkRule Nothing val
        return . IgnoreRule $ dn' `mappend` attr' `mappend` val'
        where
            mkRule x r = if isNothing x
                then maybe Done f r
                else maybe (Cont ".*" Done) f r
            f x = Delete x Done
    parseJSON _ = mzero

instance FromJSON RewriteRule where
    parseJSON (Object o) = do
        dn    <- o .:? "dn"
        attr  <- o .:? "attr"
        val   <- o .:? "value"
        let
            dn'   = mkRule (attr `mplus` val) dn
            attr' = mkRule val attr
            val'  = mkRule Nothing val
        return . RewriteRule $ dn' `mappend` attr' `mappend` val'
        where
            mkRule x r = if isNothing x
                then maybe Done f r
                else maybe (Cont ".*" Done) f r
            f x = if | [p, t] <- T.splitOn sep x -> Subst p t Done
                     | [p]    <- T.splitOn sep x -> Cont p Done
                     | otherwise -> error "Parse error in RewriteRule"
            sep = " --> "
    parseJSON _ = mzero


