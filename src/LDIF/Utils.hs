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
import qualified Data.HashMap.Lazy as HM
import Control.Arrow ((***))
import Control.Monad

toHashMap :: [LDIF] -> HM.HashMap DN LDIFRecord
toHashMap = HM.fromList

isUniqDN :: [LDIF] -> Bool
isUniqDN xs = length xs == length xs'
    where
        xs' = nubBy (\(dn, _) (dn', _) -> dn == dn') xs

bimap1 :: (a -> b) -> (a, a) -> (b, b)
bimap1 = join (***)

