--
-- <jonas.juselius@uit.no> 2014
--

module LDAPRelay.Filter (
      filterLdif
    , filterDN
    , filterEntries
    , filterEntry
) where

import LDIF
import LDAPRelay.Types
import Text.Regex.Posix
import Control.Arrow (second)
import Data.Maybe (maybe)

filterLdif :: [Filter] -> [LDIF] -> [LDIF]
filterLdif fs ldif = foldr filterMatching [] ldif
    where
        filterMatching l acc = maybe acc (:acc) $ runFilters fs l

runFilters :: [Filter] -> LDIF -> Maybe LDIF
runFilters fs l = foldl (runFilter l) fs

filterDN :: RegexStr -> LDIF -> LDIF
filterDN rex ldif = foldr filterMatching [] ldif
    where
        filterMatching x@(LDIF (dn, _)) acc =
            if rex =~ dn then x:acc else acc

filterEntries :: (DN, [Attribute]) -> [LDIF] -> [LDIF]
filterEntries spec ldif = map (filterEntry spec) ldif

filterEntry :: (DN, [Attribute]) -> LDIF -> LDIF
filterEntry (dnFilter, attrs) (LDIF x@(dn, _)) =
    if dn =~ dnFilter
    then LDIF $ second (liftLdifRecord (filterAttrs attrs)) x
    else LDIF x

filterAttrs :: [Attribute] -> [AttrSpec] -> [AttrSpec]
filterAttrs attrl = filter (not . flip elem attrl . fst)

