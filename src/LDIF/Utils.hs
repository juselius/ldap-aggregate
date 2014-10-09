{-| Helpers for working with Simple LDIF

    <jonas.juselius@uit.no> 2014
-}
{-# LANGUAGE OverloadedStrings #-}

module LDIF.Utils (
      toHashMap
    , isUniqDN
    , bimap1
) where

import LDIF.Types
import Data.List
import Data.Maybe
import qualified Data.HashMap.Lazy as HM
import Control.Arrow ((***))
import Control.Monad

toHashMap :: [LDIF] -> HM.HashMap DN LDIFRecord
toHashMap xs = HM.fromList xs

isUniqDN :: [LDIF] -> Bool
isUniqDN xs = case hasDuplicates xs of
        Just _ -> False
        Nothing -> True
    where
        hasDuplicates x = listToMaybe $
            nubBy (\(dn, _) (dn', _) -> dn == dn') x

bimap1 :: (a -> b) -> (a, a) -> (b, b)
bimap1 = join (***)

