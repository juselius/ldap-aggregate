--
-- <jonas.juselius@uit.no> 2014
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiWayIf #-}

module Aggregate.Edit (
      Criterion(..)
    , Edit
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

class (Monoid t, Eq t) => Edit t where
    edit :: (Filter a, Edit t) => [a] -> t -> t

class Filter a where
    type MatchP :: *
    matchP :: a -> MatchP -> Bool
    editP :: a -> MatchP -> MatchP
    contP :: a -> Bool

instance (Monoid v, Edit v) => Edit (HM.HashMap T.Text v) where
    edit f = HM.foldlWithKey' revise mempty
        where
            c = head f
            revise acc k v
                | matchP c k
                , False <- contP c
                , k' <- editP c k =
                    if k' == mempty
                        then acc
                        else HM.insert k' v acc
                | matchP c k
                , True <- contP c
                , k' <- editP c k
                , v' <- edit (tail f) v =
                    if | v' == mempty -> acc
                       | k' == mempty -> HM.insert k  v' acc
                       | otherwise    -> HM.insert k' v' acc
                | otherwise = HM.insert k v acc

instance Edit (HS.HashSet T.Text) where
    edit f = HS.foldl' revise mempty
        where
            c = head f
            revise acc v
                | matchP c v
                , False <- contP c
                , v' <- editP c v =
                    if v' == mempty
                        then acc
                        else HS.insert v' acc
                | otherwise = HS.insert v acc


instance Filter (Criterion T.Text) where
    type MatchP = T.Text
    matchP (criterion -> p) s = T.unpack s =~ T.unpack p
    editP _ _ = mempty
    contP (Cont _) = True
    contP _ = False

instance Filter (Criterion FromTo) where
    type MatchP = T.Text
    matchP (criterion -> (p, _)) s = T.unpack s =~ T.unpack p
    editP (criterion -> (f, t)) s = T.pack $ subRegex
        (mkRegex (T.unpack f)) (T.unpack s) (T.unpack t)
    contP (Cont _) = True
    contP _ = False

