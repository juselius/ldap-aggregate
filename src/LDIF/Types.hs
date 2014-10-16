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
    , mapLdif
    , liftLdif
    , ldapToLdif
    , makeEntryLdif
    , makeChangeLdif
) where

import LDAP.Search (LDAPEntry(..))
import LDAP.Modify (LDAPMod(..), LDAPModOp(..))
import Data.Maybe
import Data.Hashable
import Control.Arrow (second)
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S

type DN = String
type Attribute = String
type Value = String
type AttrSpec = (Attribute, [Value])

type Attrs a = M.HashMap Attribute (S.HashSet a)

type LDIF = M.HashMap DN LDIFRecord

data LDIFRecord =
      LDIFEntry {
          rDn    :: DN
        , rAttrs :: Attrs String
        }
    | LDIFChange {
          rDn   :: DN
        , rMods :: Attrs (LDAPModOp, String)
        }
    | LDIFDelete {
          rDn   :: DN
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

mapLdif :: (Attribute -> Value -> Value) -> LDIFRecord -> LDIFRecord
mapLdif f l = case l of
    LDIFEntry  _ av -> l { rAttrs = M.mapWithKey applyf  av }
    LDIFChange _ av -> l { rMods  = M.mapWithKey applyf' av }
    LDIFDelete _    -> l
    where
        applyf  k v = S.map (f k) v
        applyf' k v = S.map (second (f k)) v

liftLdif :: (DN -> DN) -> LDIFRecord -> LDIFRecord
liftLdif f l = l { rDn = f $ rDn l }

ldapToLdif :: [LDAPEntry] -> LDIF
ldapToLdif x = M.fromList $ map toll x
    where
        toll (LDAPEntry dn av) = (dn, LDIFEntry dn (M.fromList $ map toat av))
        toat (a, v) = (a, S.fromList v)

makeEntryLdif :: DN -> Attribute -> [Value]-> LDIFRecord
makeEntryLdif dn a v = LDIFEntry dn (M.singleton a (S.fromList v))

makeChangeLdif :: DN -> LDAPModOp -> Attribute -> [Value]-> LDIFRecord
makeChangeLdif dn op a v = LDIFChange dn (M.singleton a (S.fromList (zip (repeat op) v)))

