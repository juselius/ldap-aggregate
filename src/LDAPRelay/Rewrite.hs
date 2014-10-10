--
-- <jonas.juselius@uit.no> 2014
--

module LDAPRelay.Rewrite (
      rewriteDn
    , rewriteDn'
    , rewriteAttrs'
    , rewriteAttrs
    , makeRewriteDn
    , makeRewriteAttrs
) where

import LDIF
import LDAPRelay.Types
import Data.Maybe
import Data.List (foldl')
import Text.Regex.Posix
import qualified Text.RegexPR as PR
import Control.Arrow (first, second, (***))

makeRewriteDn:: [String] -> Filter FromTo
makeRewriteDn= toFilter
    where
        toFilter [f, t] = FilterDn (f, t)
        toFilter _ = InvalidFilter

makeRewriteAttrs:: [String] -> Filter FromTo
makeRewriteAttrs = toFilter
    where
        toFilter [f, t] = FilterAttr (f, t)
        toFilter [a, f, t] = FilterVal a (f, t)
        toFilter [dn, a, f, t] = FilterValDn dn a (f, t)
        toFilter _ = InvalidFilter

rewriteDn :: [Filter FromTo] -> [LDIF] -> [LDIF]
rewriteDn fs = map (rewriteDn' fs)

rewriteDn' :: [Filter FromTo] -> LDIF -> LDIF
rewriteDn' fs = g *** liftLdif' g
    where
        g = substDn fs

substDn :: [Filter FromTo] -> String -> String
substDn fts dn = fromMaybe dn getFirstMatch
    where
        getFirstMatch = listToMaybe . dropWhile (== dn) $ map doSubst fts
        doSubst (FilterDn f) = regexSub f dn
        doSubst _ = dn

rewriteAttrs :: [Filter FromTo] -> [LDIF] -> [LDIF]
rewriteAttrs fs = map (rewriteAttrs' fs)

rewriteAttrs' :: [Filter FromTo] -> LDIF -> LDIF
rewriteAttrs' fs e@(dn, _) = second (liftLdif (rewriteAttrList fs')) e
    where
        fs' = filter thisDn fs
        thisDn x = case x of
            FilterAttrDn dnf _ -> dn =~ dnf
            FilterValDn dnf _ _ -> dn =~ dnf
            _ -> True

rewriteAttrList :: [Filter FromTo] -> [AttrSpec] -> [AttrSpec]
rewriteAttrList fs = foldr (\a accr ->
    foldl' (flip rewriteAttr) a fs : accr) []

rewriteAttr :: Filter FromTo -> AttrSpec -> AttrSpec
rewriteAttr f x@(attr, _) =
    case f of
        FilterAttr ft      -> rwAttr ft x
        FilterAttrDn _ ft  -> rwAttr ft x
        FilterVal a ft     -> rwVals a ft x
        FilterValDn _ a ft -> rwVals a ft x
        _ -> x
    where
        rwAttr ft = first (regexSub ft)
        rwVals af ft = if attr =~ af then second (regexSubs ft) else id

regexSub :: FromTo -> Value -> Value
regexSub (src, dst) = PR.subRegexPR src dst

regexSubs :: FromTo -> [Value] -> [Value]
regexSubs subs = map (regexSub subs)

