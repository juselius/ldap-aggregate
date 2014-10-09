--
-- <jonas.juselius@uit.no> 2014
--

module LDAPRelay.Rewrite (
      rewriteDN
    , rewriteDN'
    , rewriteAttrs'
    , rewriteAttrs
) where

import LDAP
import LDIF
import LDAPRelay.Types
import Data.Maybe
import qualified Text.RegexPR as PR
import Control.Arrow (second)

rewriteDN :: [FromTo] -> [LDIF] -> [LDIF]
rewriteDN fts ldif = map (rewriteDN' fts) ldif

rewriteDN' :: [FromTo] -> LDIF -> LDIF
rewriteDN' fts (dn, LDIFEntry (LDAPEntry dn' x)) =
    (substDN fts dn, LDIFEntry (LDAPEntry (substDN fts dn') x))
rewriteDN' fts (dn, x) = (substDN fts dn, x)

substDN :: [FromTo] -> String -> String
substDN fts dn =
    case getFirstMatch of
        Just dn' -> dn'
        Nothing -> dn
    where
        getFirstMatch = listToMaybe . dropWhile (== dn) $
            map (flip regexSub dn) fts

rewriteAttrs :: [(DN, Attribute, FromTo)] -> [LDIF] -> [LDIF]
rewriteAttrs afts l = map (rewriteAttrs' afts) l

rewriteAttrs' :: [(DN, Attribute, FromTo)] -> LDIF -> LDIF
rewriteAttrs' afts e@(dn, l) = second (liftLdif (rewriteAttrList dn afts)) e

rewriteAttrList :: DN -> [(DN, Attribute, FromTo)] -> [AttrSpec] -> [AttrSpec]
rewriteAttrList dn rwpat attrs = map (rewriteAttr dn rwpat) attrs

rewriteAttr :: DN -> [(DN, Attribute, FromTo)] -> AttrSpec -> AttrSpec
rewriteAttr rwpat x@(attr, vals) =
    then maybe x rewrite (lookup attr rwpat)
    else x
    where
        rewrite subst = (attr, regexSubs subst vals)

regexSub :: FromTo -> Value -> Value
regexSub (src, dst) = PR.subRegexPR src dst

regexSubs :: FromTo -> [Value] -> [Value]
regexSubs subs = map (regexSub subs)

