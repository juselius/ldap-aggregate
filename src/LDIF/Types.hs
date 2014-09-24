{-# LANGUAGE LambdaCase #-}
module LDIF.Types (
      LDAPModOp(..)
    , LDAPEntry(..)
    , LDAPMod(..)
    , LDIFRecord(..)
    , LDIF(..)
    , DN
    , Attribute
    , Value
    , AttrSpec
    , isLdapEntry
    , collectLdapEntries
    , liftLdifRecord
    , entry2add
    , record2entry
) where

import LDAP.Search (LDAPEntry(..))
import LDAP.Modify (LDAPMod(..), LDAPModOp(..))

type DN = String
type Attribute = String
type Value = String
type AttrSpec = (Attribute, [Value])

data LDIFRecord =
      LDIFEntry  LDAPEntry
    | LDIFAdd    [LDAPMod]
    | LDIFChange [LDAPMod]
    | LDIFDelete
    deriving (Eq)

newtype LDIF = LDIF { ldifEntry :: (DN, LDIFRecord) } deriving (Eq)

instance Show LDIF where
    show (LDIF (dn, rec)) = case rec of
        x@(LDIFEntry _)  -> formatEntry "LDIFEntry -> " x
        x@(LDIFChange _) -> formatEntry "LDIFChange -> " x
        x@(LDIFAdd _)    -> formatEntry "LDIFAdd -> " x
        LDIFDelete       -> "LDIFDelete -> " ++ dn
        where
            formatEntry s x = s ++ "dn: " ++ dn ++ "\n" ++ show x

instance Show LDIFRecord where
    show = \case
        (LDIFEntry attrs) -> show attrs
        (LDIFAdd attrs) -> show attrs
        (LDIFChange attrs) -> show attrs
        _ -> ""

liftLdifRecord :: ([AttrSpec] -> [AttrSpec]) -> LDIFRecord -> LDIFRecord
liftLdifRecord f l = case l of
    (LDIFEntry (LDAPEntry dn av)) ->  LDIFEntry $ LDAPEntry dn (f av)
    (LDIFAdd x) -> LDIFAdd $ map applyf x
    (LDIFChange x) -> LDIFChange $ map applyf x
    LDIFDelete -> LDIFDelete
    where
        applyf (LDAPMod op a v) = LDAPMod op a $ snd $ head (f [(a, v)])

isLdapEntry :: LDIFRecord -> Bool
isLdapEntry = \case
    LDIFEntry _ -> True
    LDIFAdd _ -> True
    _ -> False

collectLdapEntries :: [LDIF] -> [LDIF]
collectLdapEntries = filter (\(LDIF (_, e)) -> isLdapEntry e)

-- | Convert LDAPEntry to a list of LDAPMod for ldapAdd
entry2add :: LDAPEntry -> (DN, [LDAPMod])
entry2add (LDAPEntry dn av) = (dn, modlist)
    where
        modlist = map (uncurry (LDAPMod LdapModAdd)) av

-- | Convert LDIFAdd to LDAPEntry
record2entry :: DN -> LDIFRecord -> Maybe LDAPEntry
record2entry dn (LDIFAdd e) = Just $ LDAPEntry dn dlm2list
    where
        dlm2list = foldr (\(LDAPMod _ a v) x -> (a, v):x) [] e
record2entry _ (LDIFEntry e) = Just e
record2entry _ _ = Nothing
