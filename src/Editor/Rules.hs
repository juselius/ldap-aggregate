--
-- <jonas.juselius@uit.no> 2014
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
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
import Text.Regex
import Text.Regex.TDFA
import Editor.Edit
import qualified Data.Text as T

newtype IgnoreRule  = IgnoreRule  (Rule T.Text) deriving (Show)
newtype RewriteRule = RewriteRule (Rule T.Text) deriving (Show)
newtype InsertRule  = InsertRule  (Rule T.Text) deriving (Show)

instance FromJSON IgnoreRule where
    parseJSON (Object o) = do
        dn    <- o .:? "dn"
        attr  <- o .:? "attr"
        value <- o .:? "value"
        let
            dnR    = maybe every final dn
            valueR = maybe Done final value
            attrR  = if isNothing value
                then maybe Done final attr
                else maybe every final attr
        return . IgnoreRule $ dnR `mappend` attrR `mappend` valueR
            where
                every = Delete ".*" Done
                final x = Delete x Done
    parseJSON _ = mzero

instance FromJSON RewriteRule where
    parseJSON (Object o) = do
        dn    <- o `getFromTo` "dn"
        attr  <- o `getFromTo` "attr"
        value <- o `getFromTo` "value"
        let
            dnR    = maybe every final dn
            valueR = maybe Done final value
            attrR  = if isNothing value
                then maybe Done final attr
                else maybe every final attr
        return . RewriteRule $ dnR `mappend` attrR `mappend` valueR
        where
            every = Subst "(.*)" "\\1" Done
            final (f, t) = Subst f t Done
            getFromTo x s = fmap parseFromTo (x .:? s)
            parseFromTo :: Maybe Value -> Maybe (T.Text, T.Text)
            parseFromTo (Just (Object x)) =
                flip parseMaybe x $ \y -> (,)
                    <$> y .: "from"
                    <*> y .: "to"
            parseFromTo _ = Nothing
    parseJSON _ = mzero


