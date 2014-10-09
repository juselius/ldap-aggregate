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
filterLdif fs ldif = map (filterLDIF fs) ldif'
    where
        ldif' = filterDN fs ldif

filterDN :: [Filter] -> [LDIF] -> [LDIF]
filterDN fs ldif = filter (\(dn, _) -> any (dnFilter dn) fs) ldif
    where
        dnFilter dn' (FilterDn f) = f =~ dn'
        dnFilter _ _ = False

filterLDIF :: [Filter] -> LDIF -> LDIF
filterLDIF fs l@(dn, x) = case x of
    LDIFEntry (LDAPEntry dn' av) ->
        (dn, LDIFEntry (LDAPEntry dn' (filterAV av)))
    {-LDIFAdd lm -> (dn, (LDIFAdd filterLM lm))-}
    {-LDIFChange lm -> (dn, (LDIFChange filterLM lm))-}
    LDIFDelete -> l
    where
        filterAV av = filter (\x -> any (runAvFilter x) fs) av
        {-filterLM lm = filter (\x -> any (runAvFilter x) fs) lm-}
        runAvFilter (a, v) (FilterDnAttr dn' at) = False
        runAvFilter (a, v) (FilterDnAttrVal dn' at val) = False
        runAvFilter (a, v) (FilterAttr at) = False
        runAvFilter _ (FilterDn _) = False


filterEntries :: (DN, [Attribute]) -> [LDIF] -> [LDIF]
filterEntries spec ldif = map (filterEntry spec) ldif

filterEntry :: (DN, [Attribute]) -> LDIF -> LDIF
filterEntry (dnFilter, attrs) x@(dn, _) =
    if dn =~ dnFilter
    then second (liftLdif (filterAttrs attrs)) x
    else x

filterAttrs :: [Attribute] -> [AttrSpec] -> [AttrSpec]
filterAttrs attrl = filter (not . flip elem attrl . fst)

