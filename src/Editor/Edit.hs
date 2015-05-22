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
    , runEdits
    ) where

import Data.Monoid
import Data.List
import Data.Hashable
import Text.Regex
import Text.Regex.TDFA
import LDIF
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

runEdits :: (Editor e, Editable v) => [e] -> v -> v
runEdits e v = foldl' (flip edit) v e

class (Monoid t, Eq t) => Editable t where
    edit :: (Editor a, Editable t) => a -> t -> t

class Editor a where
    type T  :: *
    applyR  :: a -> T -> T
    matchP  :: a -> T -> Bool
    contP   :: a -> T -> Bool
    nextR   :: a -> a

data Rule a =
      Insert { pat :: a, next :: Rule a }
    | Delete { pat :: a, next :: Rule a }
    | Subst  { pat :: a, subpat :: a, next :: Rule a }
    | Cont   { pat :: a, next :: Rule a }
    | Done
    deriving (Show, Eq, Ord)

instance (Hashable a, Monoid a) => Hashable (Rule a) where
    hashWithSalt s (Insert a b) = s `hashWithSalt` hash a + hashWithSalt s b
    hashWithSalt s (Delete a b) = s `hashWithSalt` hash a + hashWithSalt s b
    hashWithSalt s (Subst a b c) =
        s `hashWithSalt` hash (a `mappend` b) + hashWithSalt s c
    hashWithSalt s (Cont a b) = s `hashWithSalt` hash a + hashWithSalt s b
    hashWithSalt s (Done) = s `hashWithSalt` (0 :: Int)

instance Monoid (Rule a) where
    mempty = Done
    mappend a Done = a
    mappend Done a = a
    mappend a b = if atend a
        then a { next = b }
        else a { next = next a `mappend` b }
        where
            atend (next -> Done) = True
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
    type T = T.Text
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


