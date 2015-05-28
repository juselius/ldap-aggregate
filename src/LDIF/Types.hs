{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
------------------------------------------------------------------------
-- |
-- Module    : LDIF.Types
-- Copyright : Jonas Juselius 2014
--
-- The LDIF top-level type is a wrapper around two types:
--
-- * LDIFEntry represents LDIF search data, as returned from LDAP/AD
-- * LDIFMod represents LDIF change and modification operations
--
-- The LDIF wrapper is needed, since the Parser module can parse
-- entry and modification LDIF, or a mixture of both.
--
-- This module also implements explicit Show instances for the types
-- for representing the data types as textual LDIF. Internally data
-- is represented using unordered hashmaps (dictionaries) and sets,
-- for efficient lookup, difference operations and to ensure that
-- there are no duplicate entries.
------------------------------------------------------------------------
module LDIF.Types (
      LDAPModOp(..)
    , LDAPEntry(..)
    , LDAPMod(..)
    , LDIFRecord(..)
    , LDIFMod(..)
    , LDIFAttrs
    , LDIFEntries
    , LDIFMods
    , LDIFValues
    , LDIF(..)
    , DN
    , Attr
    , Value
) where

import GHC.Generics (Generic)
import Control.DeepSeq
import LDAP.Search (LDAPEntry(..))
import LDAP.Modify (LDAPMod(..), LDAPModOp(..))
import Data.Hashable
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T

type DN           = T.Text
type Attr         = T.Text
type Value        = T.Text
type LDIFEntries  = HM.HashMap DN LDIFRecord
type LDIFMods     = HM.HashMap DN LDIFMod
type LDIFValues a = HS.HashSet a
type LDIFAttrs  a = HM.HashMap Attr (LDIFValues a)

data LDIF = LDIF {
      lRec :: HM.HashMap DN LDIFRecord
    , lMod :: HM.HashMap DN LDIFMod
    } deriving (Eq)

-- This is an orphaned instance, but it's probably ok, hence the GHC
-- suppression. See the answer by Lennart Augustsson:
-- http://stackoverflow.com/questions/3079537/orphaned-instances-in-haskell
instance Hashable LDAPModOp where
    hash = fromEnum
    hashWithSalt s a = s `hashWithSalt` fromEnum a

instance NFData LDAPModOp where
    rnf x = seq x ()

data LDIFRecord = LDIFRecord  {
      rDn :: DN
    , rAttrs :: LDIFAttrs T.Text
    } deriving (Eq, Generic, NFData)

data LDIFMod
    = LDIFAdd {
          modDn :: DN
        , modAttrs :: LDIFAttrs T.Text
        }
    | LDIFChange {
          modDn :: DN
        , modMods :: LDIFAttrs (LDAPModOp, T.Text)
        }
    | LDIFDelete { modDn :: DN }
    deriving (Eq, Generic, NFData)

instance Monoid LDIFRecord where
    mempty = LDIFRecord mempty mempty
    mappend (LDIFRecord d a) (LDIFRecord d' a') =
        LDIFRecord (d `mappend` d') (a `mappend` a')

instance Show LDIF where
    show LDIF{..} = p lRec ++ p lMod
        where
            p x = unwords . map show $ HM.elems x

instance Show LDIFRecord where
    show (LDIFRecord dn av) =
        formatDn dn ++ show av

instance Show LDIFMod where
    show = \case
        LDIFAdd dn av ->
            formatDn dn ++ "changetype: add\n" ++ show av
        LDIFChange dn av ->
            formatDn dn ++ "changetype: modify\n" ++ show av
        LDIFDelete dn ->
            formatDn dn ++ "changetype: delete"

instance {-# OVERLAPS #-} Show (LDIFAttrs T.Text) where
    show = HM.foldlWithKey' showAttrs mempty
        where
            showAttrs acc k v = acc `mappend` genAttrStr k v
            genAttrStr k v = T.unpack . T.unlines . map (\x ->
                k `T.append` ": " `T.append` x) $
                  HS.toList v

instance {-# OVERLAPS #-} Show (LDIFAttrs (LDAPModOp, T.Text)) where
    show = HM.foldlWithKey' showMod mempty
        where
            showMod acc k v = acc `mappend` genModStr k v
            genModStr k v = T.unpack . T.unlines . map (\(op, x) ->
                formatOp op k
                    `T.append` k
                    `T.append` ": "
                    `T.append` x
                    `T.append` "\n-"
                ) $ HS.toList v
            formatOp LdapModAdd a =
                "add: " `T.append` a `T.append` "\n"
            formatOp LdapModDelete a =
                "delete: " `T.append` a `T.append` "\n"
            formatOp LdapModReplace a =
                "replace: " `T.append` a `T.append` "\n"
            formatOp _ a =
                "unknown: " `T.append` a `T.append` "\n"

formatDn :: DN -> String
formatDn dn = "dn: " ++ T.unpack dn ++ "\n"

