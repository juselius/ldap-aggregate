--
-- <jonas.juselius@uit.no> 2014
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
module LDIF.Editor.Edit (
      Editable(..)
    , Editor(..)
    , runEdits
    ) where

import Data.List
import Text.Regex
import Text.Regex.TDFA
import LDIF
import LDIF.Editor.Rules
-- import Control.Parallel.Strategies
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

runEdits :: (Editor e, Editable v) => [e] -> v -> v
runEdits e v = foldl' (flip edit) v e

class (Monoid t, Eq t) => Editable t where
    edit :: (Editor a, Editable t) => a -> t -> t

class Editor a where
    applyR  :: a -> T.Text -> T.Text
    matchP  :: a -> T.Text  -> Bool
    contP   :: a -> T.Text  -> Bool
    nextR   :: a -> a

instance (Monoid v, Editable v) => Editable (HM.HashMap T.Text v) where
    edit e x = HM.foldlWithKey' runEdit mempty x -- `using` parTraversable rseq
        where
            runEdit acc k v
                | matchP e k
                , not $ contP e k
                , k' <- applyR e k =
                    if k' == mempty
                        then acc
                        else HM.insert k' v acc
                | matchP e k
                , contP e k
                , k' <- applyR e k
                , v' <- edit (nextR e) v =
                    if
                        | v' == mempty -> acc
                        | k' == mempty -> acc
                        | otherwise    -> HM.insert k' v' acc
                | otherwise = HM.insert k v acc

instance Editable (HS.HashSet T.Text) where
    edit e = HS.foldl' runEdit mempty
        where
            runEdit acc v
                | matchP e v
                , v' <- applyR e v =
                    if v' == mempty
                        then acc
                        else HS.insert v' acc
                | otherwise = HS.insert v acc

instance Editable LDIFRecord where
    edit e (LDIFRecord dn r) = LDIFRecord dn' r'
        where
            r'  = edit e r
            dn' | matchP e dn
                , x <- applyR e dn =
                    if x == mempty
                        then dn
                        else x
                | otherwise = dn

instance Editor (Rule T.Text) where
    matchP r v
        | Insert p _   <- r = T.unpack v =~ T.unpack p
        | Delete p _   <- r = T.unpack v =~ T.unpack p
        | Subst  p _ _ <- r = T.unpack v =~ T.unpack p
        | Cont   p _   <- r = T.unpack v =~ T.unpack p
        | otherwise         = False
    contP r _
        | Insert _   Done <- r = False
        | Delete _   Done <- r = False
        | Subst  _ _ Done <- r = False
        | Cont   _   Done <- r = False
        | Cont   _ _      <- r = True
        | Done            <- r = False
        | otherwise            = True
    nextR r
        | Insert _ n   <- r = n
        | Delete _ n   <- r = n
        | Subst  _ _ n <- r = n
        | Cont   _ n   <- r = n
        | otherwise         = Done
    applyR r v
        | Insert t _    <- r = t
        | Delete _ Done <- r = mempty
        | Delete _ _    <- r = v
        | Cont   _ _    <- r = v
        | Subst  p s _  <- r = T.pack $ subRegex
            (mkRegex (T.unpack p)) (T.unpack v) (T.unpack s)
        | otherwise         = mempty


