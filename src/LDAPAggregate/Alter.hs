--
-- <jonas.juselius@uit.no> 2014
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiWayIf #-}

module LDAPAggregate.Alter (
      Criterion(..)
    , Alter
    , Transfigure
    , Pattern
    , FromTo
) where
import Data.Monoid
import Text.Regex
import Text.Regex.TDFA
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

type Pattern = T.Text
type FromTo = (Pattern, T.Text)

data Criterion a =
      Cont  { criterion :: a }
    | Break { criterion :: a }
    deriving (Show)

class (Monoid t, Eq t) => Alter t where
    alter :: (Transfigure a, Alter t) => [a] -> t -> t

class Transfigure a where
    type MatchP :: *
    matchP :: a -> MatchP -> Bool
    transfigureP :: a -> MatchP -> MatchP
    contP :: a -> Bool

instance (Monoid v, Alter v) => Alter (HM.HashMap T.Text v) where
    alter f = HM.foldlWithKey' transfig mempty
        where
            c = head f
            transfig acc k v
                | matchP c k
                , False <- contP c
                , k' <- transfigureP c k =
                    if k' == mempty
                        then acc
                        else HM.insert k' v acc
                | matchP c k
                , True <- contP c
                , k' <- transfigureP c k
                , v' <- alter (tail f) v =
                    if | v' == mempty -> acc
                       | k' == mempty -> HM.insert k  v' acc
                       | otherwise    -> HM.insert k' v' acc
                | otherwise = HM.insert k v acc

instance Alter (HS.HashSet T.Text) where
    alter f = HS.foldl' transfig mempty
        where
            c = head f
            transfig acc v
                | matchP c v
                , False <- contP c
                , v' <- transfigureP c v =
                    if v' == mempty
                        then acc
                        else HS.insert v' acc
                | otherwise = HS.insert v acc


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

