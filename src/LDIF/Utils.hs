{-| Helpers for working with Simple LDIF

    <jonas.juselius@uit.no> 2014
-}
{-# LANGUAGE OverloadedStrings #-}

module LDIF.Utils (
      toAssocList
    , toHashMap
    , isUniqDN
) where

import LDIF.Types
import Data.List
import Data.Maybe
import qualified Data.HashMap.Lazy as HM

toAssocList :: [LDIF] -> [(DN, LDIFRecord)]
toAssocList = map ldifEntry

toHashMap :: [LDIF] -> HM.HashMap DN LDIFRecord
toHashMap xs = HM.fromList $ map ldifEntry xs

isUniqDN :: [LDIF] -> Bool
isUniqDN xs = case hasDuplicates $ toAssocList xs of
        Just _ -> False
        Nothing -> True
    where
        hasDuplicates x = listToMaybe $
            nubBy (\(dn, _) (dn', _) -> dn == dn') x

