{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
------------------------------------------------------------------------
-- |
-- Module      : LDIF.Editor.Edit
-- Copyright   : Jonas Juselius 2015
--
-- Rules supporting insertions, deletions and rewrites with
-- recursion depth control, i.e. how many levels deep to recurse into
-- recursively defined data structures.

-- The modules also impements YAML/JSON parsers for reading Rules from
-- text data.
------------------------------------------------------------------------
module LDIF.Editor.Rules (
      Rule(..)
    , IgnoreRule(..)
    , RewriteRule(..)
    , InsertRule(..)
    ) where

import GHC.Generics (Generic)
import Data.Yaml
import Data.Maybe
import Data.Hashable
import Control.Monad
import Control.DeepSeq
import qualified Data.Text as T

data Rule a =
      Insert { pat :: a, next :: Rule a }
    | Delete { pat :: a, next :: Rule a }
    | Subst  { pat :: a, subpat :: a, next :: Rule a }
    | Cont   { pat :: a, next :: Rule a }
    | Done
    deriving (Show, Eq, Ord, Generic, NFData)

newtype IgnoreRule  = IgnoreRule  {
    doIgnore  :: Rule T.Text
    } deriving (Show, Eq, Ord)

newtype RewriteRule = RewriteRule {
    doRewrite :: Rule T.Text
    } deriving (Show, Eq, Ord)

newtype InsertRule  = InsertRule  {
    doInsert  :: Rule T.Text
    } deriving (Show, Eq, Ord)

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

instance (Hashable a, Monoid a) => Hashable (Rule a) where
    hashWithSalt s (Insert a b) = s `hashWithSalt` hash a + hashWithSalt s b
    hashWithSalt s (Delete a b) = s `hashWithSalt` hash a + hashWithSalt s b
    hashWithSalt s (Subst a b c) =
        s `hashWithSalt` hash (a `mappend` b) + hashWithSalt s c
    hashWithSalt s (Cont a b) = s `hashWithSalt` hash a + hashWithSalt s b
    hashWithSalt s (Done) = s `hashWithSalt` (0 :: Int)


instance FromJSON IgnoreRule where
    parseJSON (Object o) = do
        dn    <- o .:? "dn"
        attr  <- o .:? "attr"
        val   <- o .:? "value"
        let
            dn'   = mkRule (attr `mplus` val) dn
            attr' = mkRule val attr
            val'  = mkRule Nothing val
        return . IgnoreRule $ dn' `mappend` attr' `mappend` val'
        where
            mkRule x r = if isNothing x
                then maybe Done f r
                else maybe (Cont ".*" Done) f r
            f x = Delete x Done
    parseJSON _ = mzero

instance FromJSON RewriteRule where
    parseJSON (Object o) = do
        dn    <- o .:? "dn"
        attr  <- o .:? "attr"
        val   <- o .:? "value"
        let
            dn'   = mkRule (attr `mplus` val) dn
            attr' = mkRule val attr
            val'  = mkRule Nothing val
        return . RewriteRule $ dn' `mappend` attr' `mappend` val'
        where
            mkRule x r = if isNothing x
                then maybe Done f r
                else maybe (Cont ".*" Done) f r
            f x = if | [p, t] <- T.splitOn sep x -> Subst p t Done
                     | [p]    <- T.splitOn sep x -> Cont p Done
                     | otherwise -> error "Parse error in RewriteRule"
            sep = " --> "
    parseJSON _ = mzero


