--
-- <jonas.juselius@uit.no> 2014
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module LDIF.Types (
      LDAPModOp(..)
    , LDAPEntry(..)
    , LDAPMod(..)
    , LDIFRecord(..)
    , LDIFOper(..)
    , LDIFAttrs(..)
    , LDIFEntries
    , LDIFMods
    , LDIFValues
    , LDIF(..)
    , DN
    , LdifAttr
    , LdifValue
) where

import LDAP.Search (LDAPEntry(..))
import LDAP.Modify (LDAPMod(..), LDAPModOp(..))
import Data.Hashable
import Data.Monoid
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T

type DN           = T.Text
type LdifAttr     = T.Text
type LdifValue    = T.Text
type LDIFEntries  = HM.HashMap DN LDIFRecord
type LDIFMods     = HM.HashMap DN LDIFOper
type LDIFValues a = HS.HashSet a

-- data LDIF = LRec (HM.HashMap DN LDIFRecord) | LOp (HM.HashMap DN LDIFOper)
data LDIF = LDIF {
      lRec :: HM.HashMap DN LDIFRecord
    , lOp  :: HM.HashMap DN LDIFOper
    }

newtype LDIFAttrs a = LDIFAttrs {
    toHM :: HM.HashMap LdifAttr (LDIFValues a)
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
    }

data LDIFOper
    = LDIFAdd {
          opDn :: DN
        , opAttrs :: LDIFAttrs T.Text
        }
    | LDIFChange {
          opDn :: DN
        , opMods :: LDIFAttrs (LDAPModOp, T.Text)
        }
    | LDIFDelete { opDn :: DN }
    deriving (Eq)

instance Monoid LDIFRecord where
    mempty = LDIFRecord mempty mempty
    mappend (LDIFRecord d a) (LDIFRecord d' a') =
        LDIFRecord (d `mappend` d') (a `mappend` a')

instance Monoid (LDIFAttrs T.Text) where
    mempty = LDIFAttrs mempty
    mappend (LDIFAttrs a) (LDIFAttrs a') = LDIFAttrs (a `mappend` a')

instance Show LDIF where
    show LDIF{..} = p lRec ++ p lOp
        where
            p x = unwords . map show $ HM.elems x

instance Show LDIFRecord where
    show (LDIFRecord dn av) =
        formatDn dn ++ "\n" ++ show av

instance Show LDIFOper where
    show = \case
        LDIFAdd dn av ->
            formatDn dn ++ "changetype: add\n" ++ show av
        LDIFChange dn av ->
            formatDn dn ++ "changetype: modify\n" ++ show av
        LDIFDelete dn ->
            formatDn dn ++ "changetype: delete"

instance Show (LDIFAttrs T.Text) where
    show l = HM.foldlWithKey' showAttrs mempty $ toHM l
        where
            showAttrs acc k v = acc `mappend` genAttrStr k v
            genAttrStr k v = T.unpack . T.unlines . map (\x ->
                k `T.append` ": " `T.append` x) $
                  HS.toList v

instance Show (LDIFAttrs (LDAPModOp, T.Text)) where
    show l = HM.foldlWithKey' showMod mempty $ toHM l
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

