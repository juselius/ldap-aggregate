{-| Functions for comparing simple LDIF data

    <jonas.juselius@uit.no> 2014
-}
{-# LANGUAGE OverloadedStrings #-}

module LDIF.Compare (
      filterEqDN
) where

import LDIF.Simple
import LDIF.Utils
import Data.Maybe

filterEqDN :: [LDIF] -> [LDIF] -> [LDIF]
filterEqDN src dest =
    map LDIF filterMatching
    where
        filterMatching = filter (\x -> isNothing $ lookup (fst x) dest') src'
        src' = toAssocList src
        dest' = toAssocList dest


filterEq :: [LDIF] -> [LDIF] -> [LDIF]
filterEq src dest =
    map LDIF filterMatching
    where
        filterMatching = filter (\x -> case lookup (fst x) dest') src'
        src' = toAssocList src
        dest' = toAssocList dest
