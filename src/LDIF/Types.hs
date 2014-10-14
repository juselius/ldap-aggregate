--
-- <jonas.juselius@uit.no> 2014
--
{-# LANGUAGE LambdaCase #-}
module LDIF.Types (
      LDAPModOp(..)
    , LDAPEntry(..)
    , LDAPMod(..)
    , LDIFRecord(..)
    , LDIF
    , DN
    , Attribute
    , Value
    , AttrSpec
    , Attrs
    , liftLdif
    , liftLdif'
    --, ldapToLdif
    --, ldifToLdap
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
type Attrs a = M.HashMap Attribute (S.HashSet a)

data LDIF = LDIF (M.HashMap DN LDIFRecord)
data LDAPRecod = LDAPEntry | LDAPMod

data LDIFRecord =
      LDIFEntry {
          recDn    :: DN
        , recAttrs :: Attrs Value
        }
    | LDIFChange {
          recDn   :: DN
        , recMods :: Attrs (LDAPModOp, Value)
        }
    | LDIFDelete {
          recDn   :: DN
        } deriving (Eq)

instance Hashable LDAPModOp where
    hash = fromEnum
    hashWithSalt s a = s `hashWithSalt` fromEnum a

instance Show LDIFRecord where
    show = \case
        LDIFEntry dn av -> formatDn dn ++ "changetype: add\n"
            ++ pprint formatEntry av
        LDIFChange dn av -> formatDn dn ++ "changetype: modify\n"
            ++ pprint formatChange av
        LDIFDelete dn -> formatDn dn ++ "changetype: delete"
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

liftLdif :: (Attribute -> Value -> Value) -> LDIFRecord -> LDIFRecord
liftLdif f l = case l of
    LDIFEntry  _ av -> l { recAttrs = M.mapWithKey applyf  av }
    LDIFChange _ av -> l { recMods  = M.mapWithKey applyf' av }
    LDIFDelete _    -> l
    where
        applyf  k v = S.map (f k) v
        applyf' k v = S.map (second (f k)) v

liftLdif' :: (DN -> DN) -> LDIFRecord -> LDIFRecord
liftLdif' f l = l { recDn = f $ recDn l }

--ldapToLdif :: [a] -> LDIF
--ldap2Ldif x = map  x
    -- \case
    --LDAPEntry dn av ->

--ldifToLdap :: LDIF -> LDAP


