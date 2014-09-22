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
import LDIF.Parser
import Text.Regex.Posix
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Text.RegexPR as PR

type RegexStr = String
type FromTo = (RegexStr, RegexStr)

-- | Convert a LDIF string of LDAP search results to LDAPMod for add
ldapStr2LdapMod :: BS.ByteString -> [(String, [LDAPMod])]
ldapStr2LdapMod str =
        map toMod ldif
    where
        ldif = extractEntries $ parseLDIFStr "" str
        extractEntries =
            either (error . show) (map ldifEntry)
        toMod (dn, LDIFEntry attrs) = (dn, list2ldm LdapModAdd attrs)
        toMod (dn, LDIFUpdate op attrs) = (dn, list2ldm op attrs)
        toMod (dn, LDIFChange op attrs) = (dn, list2ldm op attrs)

rewriteDN :: [FromTo] -> LDIF -> LDIF
rewriteDN fts (LDIF (dn, x)) = LDIF (substDN fts dn, x)

substDN :: [FromTo] -> String -> String
substDN fts dn =
    case getFirstMatch of
        Just dn' -> dn'
        Nothing -> dn
    where
        getFirstMatch = listToMaybe . dropWhile (== dn) $
            map (flip regexSub dn) fts

rewriteAttr :: [(Attribute, FromTo)] -> AttrSpec -> AttrSpec
rewriteAttr afts x@(attr, vals) =
        maybe x doRewrite (lookup attr afts)
        where
            doRewrite subst = (attr, regexSubs subst vals)

rewriteAttrs :: [(Attribute, FromTo)] -> LDIF -> LDIF
rewriteAttrs afts (LDIF (dn, x)) = LDIF
    (dn, x { ldifAttrs = map (rewriteAttr afts) (ldifAttrs x) })

filterAttrs :: [Attribute] -> [AttrSpec] -> [AttrSpec]
filterAttrs attrl = filter (not . flip elem attrl . fst)

filterEntry :: (DN, [Attribute]) -> LDIF -> LDIF
filterEntry (dnFilter, attrl) x@(LDIF (dn, le)) =
    if dn =~ dnFilter
        then LDIF (dn, le { ldifAttrs =
            filterAttrs attrl (ldifAttrs le) })
        else x

regexSub :: FromTo -> Value -> Value
regexSub (src, dst) = PR.subRegexPR src dst

regexSubs :: FromTo -> [Value] -> [Value]
regexSubs subs = map (regexSub subs)

