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
type RuleF a = Maybe a -> Rule T.Text

instance FromJSON IgnoreRule where
    parseJSON (Object o) = do
        dn    <- liftM dnR   $ o .:? "dn"
        attr  <- liftM attrR $ o .:? "attr"
        value <- liftM valR  $ o .:? "value"
        return . IgnoreRule $ dn `mappend` attr `mappend` value
        where
            (dnR, attrR, valR) = mkRs glob final
            glob = Delete ".*" Done
            final x = Delete x Done
    parseJSON _ = mzero

instance FromJSON RewriteRule where
    parseJSON (Object o) = do
        dn    <- liftM dnR   $ o `getFromTo` "dn"
        attr  <- liftM attrR $ o `getFromTo` "attr"
        value <- liftM valR  $ o `getFromTo` "value"
        return . RewriteRule $ dn `mappend` attr `mappend` value
        where
            (dnR, attrR, valR) = mkRs glob final
            glob = Subst "(.*)" "\\1" Done
            final (f, t) = Subst f t Done
            getFromTo x s = fmap parseFromTo (x .:? s)
    parseJSON _ = mzero

parseFromTo :: Maybe Value -> Maybe (T.Text, T.Text)
parseFromTo (Just (Object x)) =
    flip parseMaybe x $ \y -> (,)
        <$> y .: "from"
        <*> y .: "to"
parseFromTo _ = Nothing

mkRs :: RuleT -> (a -> RuleT) -> (RuleF a, RuleF a, RuleF a)
mkRs e f = (
      maybe e f
    , maybe Done f
    , \x -> if isNothing x
              then maybe Done f x
              else maybe e f x
    )
