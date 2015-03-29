--
-- <jonas.juselius@uit.no> 2015
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}
module Config where

import Data.Maybe
import Data.Monoid
import Data.Yaml
import Text.Regex
import Text.Regex.TDFA
import Control.Applicative
import Control.Monad
import qualified Data.Text as T
import qualified SimpleLDIF as L
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S

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
    , ditIgnoreFilters :: [IgnoreCriterion]
    , ditRewriteFilters :: [RewriteCriterion]
    } deriving (Show)

data SearchBase = SearchBase {
      searchBase :: T.Text
    , searchFilter :: T.Text
    } deriving (Show)

type Pattern = T.Text
type Attribute = T.Text
type Replace = T.Text
type FromTo = (Pattern, Replace)
type IgnoreCriterion = [Criterion Pattern]
type RewriteCriterion = [Criterion FromTo]

data Criterion a =
      Cont  { criterion :: a }
    | Break { criterion :: a }
    deriving (Show)

class (Monoid t, Eq t) => Alter t where
    alter :: (Transfigure a, Alter t) => [a] -> t -> t

instance (Monoid v, Alter v) => Alter (M.HashMap T.Text v) where
    alter f l = M.foldlWithKey' transfig mempty l
        where
            c = head f
            transfig acc k v
                | matchP c k
                , False <- contP c
                , k' <- transfigureP c k =
                    if k' == mempty
                        then acc
                        else M.insert k' v acc
                | matchP c k
                , True <- contP c
                , k' <- transfigureP c k
                , v' <- alter (tail f) v =
                    case (v' == mempty, k' == mempty) of
                        (True,  True)  -> acc
                        (True,  False) -> acc
                        (False, True)  -> M.insert k  v' acc
                        (False, False) -> M.insert k' v' acc
                | otherwise = M.insert k v acc

instance Alter (L.ValueSet T.Text) where
    alter f l = S.foldl' transfig mempty l
        where
            c = head f
            transfig acc v
                | matchP c v
                , False <- contP c
                , v' <- transfigureP c v =
                    if v' == mempty
                        then acc
                        else S.insert v' acc
                | otherwise = S.insert v acc

class Transfigure a where
    type MatchP :: *
    matchP :: a -> MatchP -> Bool
    transfigureP :: a -> MatchP -> MatchP
    contP :: a -> Bool

instance Transfigure (Criterion T.Text) where
    type MatchP = T.Text
    matchP (criterion -> p) s = T.unpack s =~ T.unpack p
    transfigureP _ _ = mempty
    contP (Cont _) = True
    contP _ = False

instance Transfigure (Criterion FromTo) where
    type MatchP = T.Text
    matchP (criterion -> (p, _)) s = T.unpack s =~ T.unpack p
    transfigureP (criterion -> (f, t)) s = T.pack $ subRegex
        (mkRegex (T.unpack f)) (T.unpack s) (T.unpack t)
    contP (Cont _) = True
    contP _ = False

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
        return $ [
              (toCriterion (attr `mplus` value) dn)
            , (toCriterion value attr)
            , (toCriterion Nothing value)
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
        return $ [
              (toCriterion (attr `mplus` value) dn)
            , (toCriterion value attr)
            , (toCriterion Nothing value)
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

l0 :: M.HashMap T.Text (M.HashMap T.Text (S.HashSet T.Text))
l0 = M.fromList [
      ("l0a", l10)
    , ("l0b", l11)
    ]

l10 :: M.HashMap T.Text (S.HashSet T.Text)
l10 = M.fromList [
      ("l10a", s10a)
    , ("l10b", s10a)
    ]

l11 :: M.HashMap T.Text (S.HashSet T.Text)
l11 = M.fromList [
      ("l11a", s11a)
    , ("l11b", s11b)
    ]

s10a :: S.HashSet T.Text
s10a = S.fromList ["s10a-1", "s10a-2"]

s10b :: S.HashSet T.Text
s10b = S.fromList ["s10b-1", "s10b-2"]

s11a :: S.HashSet T.Text
s11a = S.fromList ["s11a-1", "s11a-2"]

s11b :: S.HashSet T.Text
s11b = S.fromList ["s11b-1", "s11b-2"]

f0 :: [Criterion T.Text]
f0 = [Break "l0a"]

f1 :: [Criterion T.Text]
f1 = [Cont "l0a", Break "l10a"]

f2 :: [Criterion T.Text]
f2 = [Cont "l0a", Cont "l10a", Break "s10a-1"]

r0 :: [Criterion FromTo]
r0 = [Break ("l0a", "L0A")]

r1 :: [Criterion FromTo]
r1 = [Cont ("l(0)a", "L-\\1-A"), Break ("l10a", "L10A")]

r2 :: [Criterion FromTo]
r2 = [Cont ("l0a", "L10A"), Cont ("l10a", "L10A"), Break ("s10a-1", "S10A-1")]
