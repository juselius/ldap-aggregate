--
-- <jonas.juselius@uit.no> 2014
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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

import LDAP.Search (LDAPEntry(..))
import LDAP.Modify (LDAPMod(..), LDAPModOp(..))
import Data.Hashable
import Data.Monoid
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

-- data LDIF = LRec (HM.HashMap DN LDIFRecord) | LOp (HM.HashMap DN LDIFMod)
data LDIF = LDIF {
      lRec :: HM.HashMap DN LDIFRecord
    , lOp  :: HM.HashMap DN LDIFMod
    } deriving (Eq)

-- This is an orphaned instance, but it's probably ok, hence the GHC
-- suppression. See the answer by Lennart Augustsson:
-- http://stackoverflow.com/questions/3079537/orphaned-instances-in-haskell
instance Hashable LDAPModOp where
    hash = fromEnum
    hashWithSalt s a = s `hashWithSalt` fromEnum a

data LDIFRecord = LDIFRecord  {
      rDn :: DN
    , rAttrs :: LDIFAttrs T.Text
    } deriving (Eq)

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
    deriving (Eq)

instance Monoid LDIFRecord where
    mempty = LDIFRecord mempty mempty
    mappend (LDIFRecord d a) (LDIFRecord d' a') =
        LDIFRecord (d `mappend` d') (a `mappend` a')

instance Show LDIF where
    show LDIF{..} = p lRec ++ p lOp
        where
            p x = unwords . map show $ HM.elems x

instance Show LDIFRecord where
    show (LDIFRecord dn av) =
        formatDn dn ++ "\n" ++ show av

instance Show LDIFMod where
    show = \case
        LDIFAdd dn av ->
            formatDn dn ++ "changetype: add\n" ++ show av
        LDIFChange dn av ->
            formatDn dn ++ "changetype: modify\n" ++ show av
        LDIFDelete dn ->
            formatDn dn ++ "changetype: delete"

instance Show (LDIFAttrs T.Text) where
    show = HM.foldlWithKey' showAttrs mempty
        where
            showAttrs acc k v = acc `mappend` genAttrStr k v
            genAttrStr k v = T.unpack . T.unlines . map (\x ->
                k `T.append` ": " `T.append` x) $
                  HS.toList v

instance Show (LDIFAttrs (LDAPModOp, T.Text)) where
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

