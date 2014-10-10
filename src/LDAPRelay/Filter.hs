--
-- <jonas.juselius@uit.no> 2014
--

module LDAPRelay.Filter (
      filterLdif
    , filterDn
    , filterEntries
    , makeDnFilter
    , makeAttrFilter
) where

import LDIF
import LDAPRelay.Types
import Text.Regex.Posix
import Control.Arrow (second)
import Data.List (partition, foldl')

makeDnFilter :: String -> Filter RegexStr
makeDnFilter = FilterDn

makeAttrFilter :: [String] -> Filter RegexStr
makeAttrFilter = toFilter
    where
        toFilter [a] = FilterAttr a
        toFilter [dn, a] = FilterAttrDn dn a
        toFilter [dn, a, v] = FilterValDn dn a v
        toFilter _ = InvalidFilter

filterLdif :: [Filter RegexStr] -> [LDIF] -> [LDIF]
filterLdif fs ldif = filterEntries fs ldif'
    where
        ldif' = filterDn fs' ldif
        fs' = fst $ partition isDnFilter fs

filterDn :: [Filter RegexStr] -> [LDIF] -> [LDIF]
filterDn fs = filter (\(dn, _) -> any (dnFilter dn) fs)
    where
        dnFilter dn' (FilterDn f) = f =~ dn'
        dnFilter _ _ = False

filterEntries :: [Filter RegexStr] -> [LDIF] -> [LDIF]
filterEntries fs = map (filterLDIF fs')
    where
        fs' = snd $ partition isDnFilter fs

filterLDIF :: [Filter RegexStr] -> LDIF -> LDIF
filterLDIF fs l@(dn, _) = second (liftLdif (runAttrFilters dn fs)) l

runAttrFilters :: DN -> [Filter RegexStr] -> [AttrSpec] -> [AttrSpec]
runAttrFilters dn fs av = filter (\x -> any (matchAvFilter dn x) fs') av'
    where
        av' = filter (null . snd) $ map (filterValues dn fs'') av
        (fs'', fs') = partition isValueFilter fs
        isValueFilter (FilterValDn {}) = True
        isValueFilter _ = False

matchAvFilter :: DN -> AttrSpec -> Filter RegexStr -> Bool
matchAvFilter dn (a, _) f = case f of
    FilterAttr atf -> a =~ atf
    FilterAttrDn dnf atf -> dn =~ dnf && a =~ atf
    _ -> False

filterValues :: DN -> [Filter RegexStr] -> AttrSpec -> AttrSpec
filterValues dn f as = foldl' applyFilter as f
    where
        applyFilter av (FilterValDn dnf atf valf) =
            if dn =~ dnf
            then filterAV av
            else av
            where
                filterAV e@(a, v) =
                    if a =~ atf
                    then (a, filter (=~ valf) v)
                    else e
        applyFilter acc _ = acc

isDnFilter :: Filter RegexStr -> Bool
isDnFilter (FilterDn _) = True
isDnFilter _ = False

