--
-- <jonas.juselius@uit.no> 2014
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
module Editor.Edit (
      Editable(..)
    , Editor(..)
    , Rule(..)
    ) where

import Data.Monoid
import Text.Regex
import Text.Regex.TDFA
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

class (Monoid t, Eq t) => Editable t where
    edit :: (Editor a, Editable t) => a -> t -> t

class Editor a where
    type T  :: *
    applyR  :: a -> T -> T
    matchP  :: a -> T -> Bool
    contP   :: a -> T -> Bool
    nextR   :: a -> a

data Rule a =
      Insert { pat :: a, below :: (Rule a) }
    | Delete { pat :: a, below :: (Rule a) }
    | Subst  { pat :: a, subpat :: a, below :: (Rule a) }
    | Done
    deriving (Show)

instance Monoid (Rule a) where
    mempty = Done
    mappend a Done = a
    mappend Done a = a
    mappend a b = if atend a
        then a { below = b }
        else a { below = below a `mappend` b }
        where
            atend (below -> Done) = True
            atend _ = False

instance (Monoid v, Editable v) => Editable (HM.HashMap T.Text v) where
    edit e = HM.foldlWithKey' runEdit mempty
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
                        | k' == mempty -> HM.insert k  v' acc
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


instance Editor (Rule T.Text) where
    type T = T.Text
    matchP r v
        | Insert p _   <- r = T.unpack v =~ T.unpack p
        | Delete p _   <- r = T.unpack v =~ T.unpack p
        | Subst  f _ _ <- r = T.unpack v =~ T.unpack f
        | otherwise         = False
    contP r _
        | Insert _   Done <- r = False
        | Delete _   Done <- r = False
        | Subst  _ _ Done <- r = False
        | Done            <- r = False
        | otherwise            = True
        | otherwise            = True
    nextR r
        | Insert _ n   <- r = n
        | Delete _ n   <- r = n
        | Subst  _ _ n <- r = n
        | otherwise         = Done
    applyR r v
        | Insert t _    <- r = t
        | Delete _ Done <- r = mempty
        | Delete t _    <- r = t
        | Subst  f t _  <- r = T.pack $ subRegex
            (mkRegex (T.unpack f)) (T.unpack v) (T.unpack t)
        | otherwise         = mempty


