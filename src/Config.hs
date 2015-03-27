--
-- <jonas.juselius@uit.no> 2015
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Config where

import Data.Maybe
import Data.Monoid
import Data.Yaml
import Text.Regex
import Text.Regex.TDFA
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

type Pattern = T.Text
type Replace = T.Text
type FromTo = (Pattern, Replace)

data Action a =
      Cont { act :: a }
    | Use  { act :: a } deriving (Show)

class Filter a where
    matchF :: Record b => a -> b -> Bool
    applyF :: Record b => a -> b -> b

class Record a where
    type SubRec :: *
    attrR     :: a -> T.Text
    valueR    :: a -> SubRec
    rewriteR  :: Filter b => a -> b -> a

instance Record T.Text where
    type SubRec = T.Text
    attrR = id
    valueR a = a
    rewriteR = flip applyF

instance Filter (Action T.Text) where
    matchF pat rc = r =~ p
        where
            r = T.unpack $ attrR rc
            p = T.unpack $ act pat
    applyF p c = undefined

instance Filter (Action FromTo) where
    matchF = undefined
    applyF p c = undefined
    -- applyF pat rc = let (f, t) = act pat in T.pack $ subRegex f r t
    --     where
    --         r = T.unpack $ attrR rc
    --         p = mkRegex . T.unpack

data IgnoreFilter = Ignore {
      igDn    :: Action Pattern
    , igAttr  :: Action Pattern
    , igValue :: Action Pattern
    } deriving (Show)


data RewriteFilter = Rewrite {
      rwDn    :: Action FromTo
    , rwAttr  :: Action FromTo
    , rwValue :: Action FromTo
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
    parseJSON (Object o) = do
        dn    <- o .:? "dn"
        attr  <- o .:? "attr"
        value <- o .:? "value"
        return $ Ignore
            (toAction (attr `mplus` value) dn)
            (toAction value attr)
            (toAction Nothing value)
        where
            toAction :: Maybe a -> Maybe Pattern -> Action Pattern
            toAction p v
                | isJust p = Cont v'
                | otherwise = Use v'
                where v' = fromMaybe mempty v
    parseJSON _ = mzero

noMatch :: (T.Text, T.Text)
noMatch = ("^$", "")

instance FromJSON RewriteFilter where
    parseJSON (Object o) = do
        dn    <- o `getFromTo` "dn"
        attr  <- o `getFromTo` "attr"
        value <- o `getFromTo` "value"
        return $ Rewrite
            (toAction (attr `mplus` value) dn)
            (toAction value attr)
            (toAction Nothing value)
        where
            getFromTo x s = fmap parseFromTo (x .:? s)
            toAction :: Maybe a -> Maybe FromTo -> Action FromTo
            toAction p v
                | isJust p = Cont v'
                | otherwise = Use v'
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
