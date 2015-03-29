--
-- <jonas.juselius@uit.no> 2014
--
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SimpleLDIF.Types (
      LDAPModOp(..)
    , LDAPEntry(..)
    , LDAPMod(..)
    , LDIFRecord(..)
    , LDIF
    , Ldif
    , DN
    , LdifAttr
    , LdifValue
    , LdifAttrs
    , LdifValues
) where

import LDAP.Search (LDAPEntry(..))
import LDAP.Modify (LDAPMod(..), LDAPModOp(..))
import Data.Hashable
import Control.Arrow (second)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T

type DN = T.Text
type LdifAttr = T.Text
type LdifValue = T.Text
type Ldif = (DN, LDIFRecord)
type LDIF = HM.HashMap DN LDIFRecord
type LdifAttrs a = HM.HashMap LdifAttr (LdifValues a)
type LdifValues a = HS.HashSet a

-- This is an orphaned instance, but it's probably ok, hence the GHC
-- suppression. See the answer by Lennart Augustsson:
-- http://stackoverflow.com/questions/3079537/orphaned-instances-in-haskell
instance Hashable LDAPModOp where
    hash = fromEnum
    hashWithSalt s a = s `hashWithSalt` fromEnum a

data LDIFRecord
    = LDIFAdd
        { rDn :: DN
        , rAttrs :: LdifAttrs T.Text
        }
    | LDIFChange
        { rDn :: DN
        , rMods :: LdifAttrs (LDAPModOp, T.Text)
        }
    | LDIFDelete { rDn :: DN }
    deriving (Eq)

instance Show LDIFRecord where
    show = \case
        LDIFAdd dn av ->
            formatDn dn ++ "changetype: add\n"
            ++ pprint formatEntry av
        LDIFChange dn av ->
            formatDn dn ++ "changetype: modify\n"
            ++ pprint formatChange av
        LDIFDelete dn ->
            formatDn dn ++ "changetype: delete"
        where
            pprint f av = concatMap f . map (second HS.toList) $ HM.toList av
            formatEntry (a, v) = showAttrs $ zip (repeat a) v
            formatChange (a, v) = showMod $ zip (repeat a) v
            showAttrs = unlines . map (\(a, v) ->
                T.unpack a ++ ": " ++ T.unpack v
                )
            showMod = unlines . map (\(a, (op, v)) ->
                formatOp op (T.unpack a)
                    ++ T.unpack a
                    ++ ": "
                    ++ T.unpack v
                    ++ "\n"
                )
                where
                    formatOp LdapModAdd a = "add: " ++ a ++ "\n"
                    formatOp LdapModDelete a = "delete: " ++ a ++ "\n"
                    formatOp LdapModReplace a = "replace: " ++ a ++ "\n"
                    formatOp _ a = "unknown: " ++ a ++ "\n"
            formatDn dn = "dn: " ++ T.unpack dn ++ "\n"

