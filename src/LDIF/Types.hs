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
    , isLdapEntry
    , collectLdapEntries
    , ldapEntry2Add
    , ldifEntry2Add
    , ldapEntry2LDIF
    , ldifRecord2Entry
) where

import LDAP.Search (LDAPEntry(..))
import LDAP.Modify (LDAPMod(..), LDAPModOp(..))

type DN = String
type Attribute = String
type Value = String
type AttrSpec = (Attribute, [Value])
type LDIF = (DN, LDIFRecord)

data LDIFRecord =
      LDIFEntry  LDAPEntry
    | LDIFAdd    [LDAPMod]
    | LDIFChange [LDAPMod]
    | LDIFDelete
    deriving (Eq)

instance Show LDIFRecord where
    show = \case
        (LDIFEntry (LDAPEntry dn av)) -> "dn: " ++ dn ++ "\n"
            ++ (unlines . concatMap pprint $ av)
        (LDIFAdd mods) -> unlines . concatMap pprintAdd $ mods
        (LDIFChange mods) -> pprintChange mods
        _ -> ""
        where
            pprint (a, v) = map (curry printAttrs a) v
            pprintAdd (LDAPMod _ a v) = map (curry printAttrs a) v
            pprintChange mods = unlines . map printMod $ mods
            printAttrs (a, v) = a ++ ": " ++ v
            printMod (LDAPMod op a v) = printOp op a
                ++ (unlines . map printAttrs $ zip (repeat a) v) ++ "-"
            printOp LdapModAdd a = "add: " ++ a ++ "\n"
            printOp LdapModDelete a = "delete: " ++ a ++ "\n"
            printOp LdapModReplace a = "replace: " ++ a ++ "\n"
            printOp _ a = "unknown: " ++ a ++ "\n"

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

isLdapEntry :: LDIFRecord -> Bool
isLdapEntry = \case
    LDIFEntry _ -> True
    LDIFAdd _ -> True
    _ -> False

collectLdapEntries :: [LDIF] -> [LDIF]
collectLdapEntries = filter (\(_, e) -> isLdapEntry e)

-- | Convert any LDIFEntry in LDIF to LDIFAdd
ldifEntry2Add :: LDIF -> LDIF
ldifEntry2Add (dn, LDIFEntry e) = (dn, LDIFAdd (ldapEntry2Add e))
ldifEntry2Add l = l

-- | Convert LDAPEntry to a list of LDAPMod for ldapAdd
ldapEntry2Add :: LDAPEntry -> [LDAPMod]
ldapEntry2Add (LDAPEntry _ av) = map (uncurry (LDAPMod LdapModAdd)) av

ldapEntry2LDIF :: LDAPEntry -> LDIF
ldapEntry2LDIF e@(LDAPEntry dn _) = (dn, LDIFEntry e)

-- | Convert LDIFAdd to LDAPEntry
ldifRecord2Entry :: DN -> LDIFRecord -> Maybe LDAPEntry
ldifRecord2Entry dn (LDIFAdd e) = Just $ LDAPEntry dn dlm2list
    where
        dlm2list = foldr (\(LDAPMod _ a v) x -> (a, v):x) [] e
ldifRecord2Entry _ (LDIFEntry e) = Just e
ldifRecord2Entry _ _ = Nothing

