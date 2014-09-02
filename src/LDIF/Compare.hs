{-| Functions for comparing simple LDIF data

    <jonas.juselius@uit.no> 2014
-}
{-# LANGUAGE OverloadedStrings #-}

module LDIF.Compare (
      diffLdifDN
) where

import LDIF.Simple
import LDIF.Utils
import Data.List
import Data.Maybe

diffLdifDN :: [LDIF] -> [LDIF] -> Maybe [LDIF]
diffLdifDN src dest =
    case diff of
       _:_ -> Just (map LDIF diff)
       _ -> Nothing
    where
        diff = catMaybes $ map (flip lookup dest' . fst) src'
        src' = toAssocList src
        dest' = toAssocList dest

