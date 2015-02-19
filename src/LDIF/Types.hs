--
-- <jonas.juselius@uit.no> 2014
--
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module LDIF.Types (
      LDAPModOp(..)
    , LDAPEntry(..)
    , LDAPMod(..)
    , LDIFRecord(..)
    , LDIF
    , Ldif
    , DN
    , Attribute
    , Value
    , ValueSet
    , AttrSpec
    , Attrs
) where

import LDAP.Search (LDAPEntry(..))
import LDAP.Modify (LDAPMod(..), LDAPModOp(..))
import Data.Hashable
import Control.Arrow (second)
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S

type DN = String
type Attribute = String
type Value = String
type AttrSpec = (Attribute, [Value])
type Attrs a = M.HashMap Attribute (ValueSet a)
type ValueSet a = S.HashSet a
type Ldif = (DN, LDIFRecord)
type LDIF = M.HashMap DN LDIFRecord

-- This is an orphaned instance, but it's probably ok, hence the GHC
-- suppression. See the answer by Lennart Augustsson:
-- http://stackoverflow.com/questions/3079537/orphaned-instances-in-haskell
instance Hashable LDAPModOp where
    hash = fromEnum
    hashWithSalt s a = s `hashWithSalt` fromEnum a

data LDIFRecord
    = LDIFEntry { rDn :: DN
                , rAttrs :: Attrs String }
    | LDIFChange { rDn :: DN
                 , rMods :: Attrs (LDAPModOp, String) }
    | LDIFDelete { rDn :: DN }
    deriving (Eq)

instance Show LDIFRecord where
    show = \case
        LDIFEntry dn av ->
            formatDn dn ++ "changetype: add\n"
            ++ pprint formatEntry av
        LDIFChange dn av ->
            formatDn dn ++ "changetype: modify\n"
            ++ pprint formatChange av
        LDIFDelete dn ->
            formatDn dn ++ "changetype: delete"
        where
            pprint f av = concatMap f . map (second S.toList) $ M.toList av
            formatEntry (a, v) = showAttrs $ zip (repeat a) v
            formatChange (a, v) = showMod $ zip (repeat a) v
            showAttrs = unlines . map (\(a, v) -> a ++ ": " ++ v)
            showMod = unlines . map (\(a, (op, v)) ->
                formatOp op a ++ a ++ ": " ++ v ++ "\n-")
                where
                    formatOp LdapModAdd a = "add: " ++ a ++ "\n"
                    formatOp LdapModDelete a = "delete: " ++ a ++ "\n"
                    formatOp LdapModReplace a = "replace: " ++ a ++ "\n"
                    formatOp _ a = "unknown: " ++ a ++ "\n"
            formatDn dn = "dn: " ++ dn ++ "\n"

