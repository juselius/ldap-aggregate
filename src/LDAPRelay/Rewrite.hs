--
-- <jonas.juselius@uit.no> 2014
--

module LDAPRelay.Rewrite (
      rewriteDN
    , rewriteAttrs
    , filterAttrs
    , filterEntry
    , ldapRec2LdapAdd
) where

import LDAP
import LDIF.Simple
import Text.Regex.Posix
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Text.RegexPR as PR

type RegexStr = String
type FromTo = (RegexStr, RegexStr)

-- | Convert a LDIF string of LDAP search results to LDAPMod for add
ldapRec2LdapAdd :: BS.ByteString -> [(String, [LDAPMod])]
ldapRec2LdapAdd str =
        map toMod ldif
    where
        ldif = extractEntries $ parseLDIFStr "" str
        extractEntries =
            either (error . show) (map ldifEntry)
        toMod (LDIFEntry op dn attrs) = (dn, list2ldm op attrs)

rewriteDN :: [FromTo] -> LDIFEntry -> LDIFEntry
rewriteDN fts l@(LDIFEntry _ dn _) = l { ldifDN = substDN fts dn }

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

rewriteAttrs :: [(Attribute, FromTo)] -> LDIFEntry -> LDIFEntry
rewriteAttrs afts  x@(LDIFEntry _ _ attrs) =
    x { ldifAttrs = map (rewriteAttr afts) attrs }

filterAttrs :: [Attribute] -> [AttrSpec] -> [AttrSpec]
filterAttrs attrl = filter (not . flip elem attrl . fst)

filterEntry :: (String, [Attribute]) -> LDIFEntry -> LDIFEntry
filterEntry (dnFilter, attrl) ldif@(LDIFEntry _ dn attrs) =
    if dn =~ dnFilter
        then ldif { ldifAttrs = filterAttrs attrl attrs }
        else ldif

regexSub :: FromTo -> Value -> Value
regexSub (src, dst) = PR.subRegexPR src dst

regexSubs :: FromTo -> [Value] -> [Value]
regexSubs subs = map (regexSub subs)

