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
    , liftLdif
    , liftLdif'
    , showLDIF
    , ldapEntryToAdd
    , ldifEntryToAdd
    , ldapEntryToLDIF
    , ldifRecordToEntry
) where

import LDAP.Search (LDAPEntry(..))
import LDAP.Modify (LDAPMod(..), LDAPModOp(..))
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S

type DN = String
type Attribute = String
type Value = String
type AttrSpec = (Attribute, [Value])

data LDIF = LDIF (M.HashMap DN LDIFRecord)

data LDIFRecord =
      LDIFEntry {
          recDn :: DN
        , recAttrs :: M.HashMap Attribute (S.HashSet Value)
        }
    | LDIFChange {
          recDn :: DN
        , recMod :: M.HashMap Attribute (M.HashMap LDAPModOp Value)
        }
    | LDIFDelete { recDn :: DN } deriving (Eq)

instance Show LDIFRecord where
    show = \case
        LDIFEntry dn av -> "dn: " ++ dn ++ "\n"
            ++ (unlines . concatMap pprint $ av)
        LDIFChange dn  -> pprintChange mods
        _ -> ""
        where
            pprint av = map printAttrs $ H.toList av
            pprintChange mods = unlines . map printMod $ mods
            printAttrs (a, v) = a ++ ": " ++ v
            printMod (LDAPMod op a v) = printOp op a
                ++ (unlines . map printAttrs $ zip (repeat a) v) ++ "-"
            printOp LdapModAdd a = "add: " ++ a ++ "\n"
            printOp LdapModDelete a = "delete: " ++ a ++ "\n"
            printOp LdapModReplace a = "replace: " ++ a ++ "\n"
            printOp _ a = "unknown: " ++ a ++ "\n"

cmpEntry :: LDAPEntry -> LDAPEntry -> Bool
cmpEntry (LDAPEntry _ e1) (LDAPEntry _ e2) = all cmp e1
    where
        cmp (a, v) = maybe False (all (`elem` v)) $ lookup a e2

cmpMod :: [LDAPMod] -> [LDAPMod] -> Bool
cmpMod e1 e2 = (length e1 == length e2) && and (zipWith cmp e1 e2)
    where
        cmp (LDAPMod o1 a1 v1) (LDAPMod o2 a2 v2) = (o1 == o2)
            && (a1 == a2)
            && all (`elem` v2) v1


showLDIF :: LDIF -> String
showLDIF (dn, r) = case r of
        LDIFEntry _  -> formatEntry [] [] r
        LDIFChange _ -> formatEntry dn "modify" r
        LDIFAdd _    -> formatEntry dn "add" r
        LDIFDelete   -> formatEntry dn "delete" r
        where
            formatEntry dn' s x =
                   (if null dn' then "" else "dn: " ++ dn' ++ "\n")
                ++ (if null s then "" else "changetype: " ++ s ++ "\n")
                ++ show x

liftLdif :: ([AttrSpec] -> [AttrSpec]) -> LDIFRecord -> LDIFRecord
liftLdif f l = case l of
    (LDIFEntry (LDAPEntry dn av)) ->  LDIFEntry $ LDAPEntry dn (f av)
    (LDIFAdd x) -> LDIFAdd $ map applyf x
    (LDIFChange x) -> LDIFChange $ map applyf x
    LDIFDelete -> LDIFDelete
    where
        applyf (LDAPMod op a v) = LDAPMod op a $ snd $ head (f [(a, v)])

liftLdif' :: (DN -> DN) -> LDIFRecord -> LDIFRecord
liftLdif' f l = case l of
    (LDIFEntry (LDAPEntry dn av)) ->  LDIFEntry $ LDAPEntry (f dn) av
    _ -> l

-- | Convert any LDIFEntry in LDIF to LDIFAdd
ldifEntryToAdd :: LDIF -> LDIF
ldifEntryToAdd (dn, LDIFEntry e) = (dn, LDIFAdd (ldapEntryToAdd e))
ldifEntryToAdd l = l

-- | Convert LDAPEntry to a list of LDAPMod for ldapAdd
ldapEntryToAdd :: LDAPEntry -> [LDAPMod]
ldapEntryToAdd (LDAPEntry _ av) = map (uncurry (LDAPMod LdapModAdd)) av

ldapEntryToLDIF :: LDAPEntry -> LDIF
ldapEntryToLDIF e@(LDAPEntry dn _) = (dn, LDIFEntry e)

-- | Convert LDIFAdd to LDAPEntry
ldifRecordToEntry :: DN -> LDIFRecord -> Maybe LDAPEntry
ldifRecordToEntry dn (LDIFAdd e) = Just $ LDAPEntry dn dlm2list
    where
        dlm2list = foldr (\(LDAPMod _ a v) x -> (a, v):x) [] e
ldifRecordToEntry _ (LDIFEntry e) = Just e
ldifRecordToEntry _ _ = Nothing

