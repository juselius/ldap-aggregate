{-| Functions for comparing simple LDIF data

    <jonas.juselius@uit.no> 2014
-}
{-# LANGUAGE OverloadedStrings #-}

module LDIF.Compare (
      filterEqDN
    , filterEq
    , filterAttrs
) where

import LDIF.Simple
import LDIF.Utils
import Data.List
import Data.Maybe

type AttrVals a b = [(a, [b])]

filterEqDN :: [LDIF] -> [LDIF] -> [LDIF]
filterEqDN from to =
    map LDIF $ filter (isNothing . flip lookup (toAL to) . fst) (toAL from)

filterEq :: [LDIF] -> [LDIF] -> [LDIF]
filterEq from to =
    map LDIF $ filter (\(dn, rec) ->
        maybe False (== rec) $ lookup dn (toAL to)) (toAL from)

filterAttrs :: (Eq a, Eq b) => AttrVals a b -> AttrVals a b -> AttrVals a b
filterAttrs from to = missing from to ++ diffs
    where
        differ = filter (\(a, v) ->
            v /= fromJust (lookup a to)) (from \\ missing from to)
        diffs = map (\(a,v) ->
            (a, filterValues (fromJust $ lookup a to) v)) differ

missing :: (Eq a, Eq b) => AttrVals a b -> AttrVals a b -> AttrVals a b
missing from to = filter (\(a, _) -> isNothing $ lookup a to) from

filterValues :: (Eq a) => [a] -> [a] -> [a]
filterValues from = filter (`notElem` from)

toAL :: [LDIF] -> [(DN, LDIFRecord)]
toAL = toAssocList

testA = [(1, [1,2,3]), (2,[2]), (3, [3,4,5])] :: AttrVals Int Int
testB = [(1, [1,2]), (2,[2])] :: AttrVals Int Int

