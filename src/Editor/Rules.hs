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



-- instance FromJSON IgnoreRule where
--     parseJSON (Object o) = do
--         dn    <- o .:? "dn" .:! ".*"
--         attr  <- o .:? "attr"
--         value <- o .:? "value"
--         return $ IgnoreRule (Delete dn (Delete attr (Delete value Done)))
--              toRule (attr `mplus` value) dn
--              toRule value attr
--              toRule Nothing value
--         where
--             toRule :: Maybe a -> Maybe T.Text -> Rule T.Text
--             toRule p v
--                 | isJust p = Cont v'
--                 | otherwise = Break v'
--                 where v' = fromMaybe mempty v
--     parseJSON _ = mzero

-- instance FromJSON RewriteRule where
--     parseJSON (Object o) = do
--         dn    <- o `getFromTo` "dn"
--         attr  <- o `getFromTo` "attr"
--         value <- o `getFromTo` "value"
--         return $ RewriteRule [
--               toCriterion (attr `mplus` value) dn
--             , toCriterion value attr
--             , toCriterion Nothing value
--             ]
--         where
--             getFromTo x s = fmap parseFromTo (x .:? s)
--             toCriterion :: Maybe a -> Maybe FromTo -> Criterion FromTo
--             toCriterion p v
--                 | isJust p = Cont v'
--                 | otherwise = Break v'
--                 where v' = fromMaybe noMatch v
--             parseFromTo :: Maybe Value -> Maybe FromTo
--             parseFromTo (Just (Object x)) =
--                 flip parseMaybe x $ \y -> (,)
--                     <$> y .: "from"
--                     <*> y .: "to"
--             parseFromTo _ = Nothing
--             noMatch = ("^$", "")
--     parseJSON _ = mzero


