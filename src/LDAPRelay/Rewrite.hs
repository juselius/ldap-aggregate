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

rewriteAttrs :: [(Attribute, FromTo)] -> [LDIF] -> [LDIF]
rewriteAttrs afts l = map (rewriteAttrs' afts) l

rewriteAttrList :: [(Attribute, FromTo)] -> [AttrSpec] -> [AttrSpec]
rewriteAttrList rwpat attrs = map (rewriteAttr rwpat) attrs

rewriteAttr :: [(Attribute, FromTo)] -> AttrSpec -> AttrSpec
rewriteAttr rwpat x@(attr, vals) = maybe x rewrite (lookup attr rwpat)
    where
        rewrite subst = (attr, regexSubs subst vals)

rewriteAttrs' :: [(Attribute, FromTo)] -> LDIF -> LDIF
rewriteAttrs' afts l = second (liftLdif (rewriteAttrList afts)) l

regexSub :: FromTo -> Value -> Value
regexSub (src, dst) = PR.subRegexPR src dst

regexSubs :: FromTo -> [Value] -> [Value]
regexSubs subs = map (regexSub subs)

