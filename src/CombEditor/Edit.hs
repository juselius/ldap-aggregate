--
-- <jonas.juselius@uit.no> 2014
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiWayIf #-}

module CombEditor.Edit (
      Rule(..)
    , Editable
    , Editor
    , edit
) where

import Data.Monoid
import Text.Regex
import Text.Regex.TDFA
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

data Rule a =
      Insert a (Rule a)
    | Delete a (Rule a)
    | Subst a a (Rule a)
    | Done
    deriving (Show)

class (Monoid t, Eq t) => Editable t where
    edit :: (Editor a, Editable t) => a -> t -> t

class Editor a where
    type T  :: *
    subst   :: a -> T -> T
    matchP  :: a -> T -> Bool
    contP   :: a -> T -> Bool
    next    :: a -> a

instance (Monoid v, Editable v) => Editable (HM.HashMap T.Text v) where
    edit e = HM.foldlWithKey' runEdit mempty
        where
            runEdit acc k v
                | matchP e k
                , not $ contP e k
                , k' <- subst e k =
                    if k == mempty
                        then acc
                        else HM.insert k' v acc
                | matchP e k
                , contP e k
                , k' <- subst e k
                , v' <- edit (next e) v =
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
                , v' <- subst e v =
                    if v' == mempty
                        then acc
                        else HS.insert v' acc
                | otherwise = HS.insert v acc


instance Editor (Rule T.Text) where
    type T = T.Text
    subst r v
        | Insert t _   <- r = t
        | Delete _ Done   <- r = mempty
        | Delete t _      <- r = t
        | Subst  f t _ <- r = T.pack $ subRegex
            (mkRegex (T.unpack f)) (T.unpack v) (T.unpack t)
        | otherwise         = mempty
    matchP r v
        | Insert p _   <- r = T.unpack v =~ T.unpack p
        | Delete p _   <- r = T.unpack v =~ T.unpack p
        | Subst  f _ _ <- r = T.unpack v =~ T.unpack f
        | otherwise         = False
    contP r _
        | Done         <- r = False
        | otherwise         = True
    next r
        | Insert _ n   <- r = n
        | Delete _ n   <- r = n
        | Subst  _ _ n <- r = n
        | otherwise         = Done


