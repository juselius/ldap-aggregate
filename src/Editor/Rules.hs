--
-- <jonas.juselius@uit.no> 2014
--
{-# LANGUAGE OverloadedStrings #-}
module Editor.Rules (
      Rule(..)
    , IgnoreRule(..)
    , RewriteRule(..)
    , InsertRule(..)
    ) where

import Data.Yaml
import Data.Maybe
import Data.Monoid
import Control.Applicative
import Control.Monad
import Editor.Edit
import qualified Data.Text as T

newtype IgnoreRule  = IgnoreRule  (Rule T.Text) deriving (Show, Eq)
newtype RewriteRule = RewriteRule (Rule T.Text) deriving (Show, Eq)
newtype InsertRule  = InsertRule  (Rule T.Text) deriving (Show, Eq)

type RuleT = Rule T.Text

instance FromJSON IgnoreRule where
    parseJSON (Object o) = do
        dn    <- o .:? "dn"
        attr  <- o .:? "attr"
        val   <- o .:? "value"
        let
            dn'   = mkRule glob stencil (attr `mplus` val) dn
            attr' = mkRule glob stencil val attr
            val'  = mkRule glob stencil Nothing val
        return . IgnoreRule $ dn' `mappend` attr' `mappend` val'
        where
            glob = Delete ".*" Done
            stencil x = Delete x Done
    parseJSON _ = mzero

instance FromJSON RewriteRule where
    parseJSON (Object o) = do
        dn    <- o `getFromTo` "dn"
        attr  <- o `getFromTo` "attr"
        val <- o `getFromTo` "value"
        let
            dn'   = mkRule glob stencil (attr `mplus` val) dn
            attr' = mkRule glob stencil val attr
            val'  = mkRule glob stencil Nothing val
        return . RewriteRule $ dn' `mappend` attr' `mappend` val'
        where
            glob = Subst "(.*)" "\\1" Done
            stencil (f, t) = Subst f t Done
            getFromTo x s = fmap parseFromTo (x .:? s)
    parseJSON _ = mzero

parseFromTo :: Maybe Value -> Maybe (T.Text, T.Text)
parseFromTo (Just (Object x)) =
    flip parseMaybe x $ \y -> (,)
        <$> y .: "from"
        <*> y .: "to"
parseFromTo _ = Nothing

mkRule :: RuleT -> (a -> RuleT) -> Maybe a -> Maybe a -> RuleT
mkRule e s x t = if isNothing x
      then maybe Done s t
      else maybe e s t

