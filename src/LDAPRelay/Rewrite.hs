--
-- <jonas.juselius@uit.no> 2014
--

module LDAPRelay.Rewrite (
      rewriteDN
    , rewriteAttrs
    , filterAttrs
    , filterEntry
    , ldapStr2LdapMod
) where

import LDAP
import LDIF
import Text.Regex.Posix
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Text.RegexPR as PR
import Control.Arrow (second)

type RegexStr = String
type FromTo = (RegexStr, RegexStr)

-- | Convert a LDIF string of LDAP search results to LDAPMod for add
ldapStr2LdapMod :: BS.ByteString -> [(String, [LDAPMod])]
ldapStr2LdapMod str =
        map toMod ldif
    where
        ldif = extractEntries $ parseLDIFStr "" str
        extractEntries = either (error . show) (map ldifEntry)
        toMod (dn, LDIFEntry x) = (dn, snd (entry2add x))
        toMod (dn, LDIFAdd x) = (dn, x)
        toMod (dn, _) = (dn, [])

rewriteDN :: [FromTo] -> LDIF -> LDIF
rewriteDN fts (LDIF (dn, LDIFEntry (LDAPEntry dn' x))) =
    LDIF (substDN fts dn, LDIFEntry (LDAPEntry (substDN fts dn') x))
rewriteDN fts (LDIF (dn, x)) = LDIF (substDN fts dn, x)

substDN :: [FromTo] -> String -> String
substDN fts dn =
    case getFirstMatch of
        Just dn' -> dn'
        Nothing -> dn
    where
        getFirstMatch = listToMaybe . dropWhile (== dn) $
            map (flip regexSub dn) fts

rewriteAttrList :: [(Attribute, FromTo)] -> [AttrSpec] -> [AttrSpec]
rewriteAttrList rwpat attrs = map (rewriteAttr rwpat) attrs

rewriteAttr :: [(Attribute, FromTo)] -> AttrSpec -> AttrSpec
rewriteAttr rwpat x@(attr, vals) = maybe x rewrite (lookup attr rwpat)
    where
        rewrite subst = (attr, regexSubs subst vals)

rewriteAttrs :: [(Attribute, FromTo)] -> LDIF -> LDIF
rewriteAttrs afts (LDIF l) =
    LDIF $ second (liftLdifRecord (rewriteAttrList afts)) l

filterAttrs :: [Attribute] -> [AttrSpec] -> [AttrSpec]
filterAttrs attrl = filter (not . flip elem attrl . fst)

filterEntry :: (DN, [Attribute]) -> LDIF -> LDIF
filterEntry (dnFilter, attrs) (LDIF x@(dn, _)) =
    if dn =~ dnFilter
    then LDIF $ second (liftLdifRecord (filterAttrs attrs)) x
    else LDIF x

regexSub :: FromTo -> Value -> Value
regexSub (src, dst) = PR.subRegexPR src dst

regexSubs :: FromTo -> [Value] -> [Value]
regexSubs subs = map (regexSub subs)

