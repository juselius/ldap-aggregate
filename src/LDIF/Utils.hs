{-| Helpers for working with Simple LDIF

    <jonas.juselius@uit.no> 2014
-}

module LDIF.Utils (
      bimap1
) where

import Control.Arrow ((***))
import Control.Monad

bimap1 :: (a -> b) -> (a, a) -> (b, b)
bimap1 = join (***)

