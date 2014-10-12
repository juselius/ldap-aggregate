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
makeAttrFilter [a] = FilterAttr a
makeAttrFilter [dn, a] = FilterAttrDn dn a
makeAttrFilter [dn, a, v] = FilterValDn dn a v
makeAttrFilter _ = InvalidFilter

filterLdif :: [Filter RegexStr] -> [LDIF] -> [LDIF]
filterLdif fs ldif = filterEntries fs ldif'
    where
        ldif' = filterDn fs' ldif
        fs' = fst $ partition isDnFilter fs

filterDn :: [Filter RegexStr] -> [LDIF] -> [LDIF]
filterDn fs = filter (\(dn, _) -> all (dnFilter dn) fs)
    where
        dnFilter dn' (FilterDn f) = not $ dn' =~ f
        dnFilter _ _ = True

filterEntries :: [Filter RegexStr] -> [LDIF] -> [LDIF]
filterEntries fs = map (filterLDIF fs')
    where
        fs' = snd $ partition isDnFilter fs

filterLDIF :: [Filter RegexStr] -> LDIF -> LDIF
filterLDIF fs l@(dn, _) = second (liftLdif (runAttrFilters dn fs)) l

runAttrFilters :: DN -> [Filter RegexStr] -> [AttrSpec] -> [AttrSpec]
runAttrFilters dn fs av = filter (\x -> all (matchAttrFilter dn x) fs') av'
    where
        av' = filter (not . null . snd) $ map (filterValues dn fs'') av
        (fs'', fs') = partition isValueFilter fs
        isValueFilter (FilterVal {}) = True
        isValueFilter (FilterValDn {}) = True
        isValueFilter _ = False

matchAttrFilter :: DN -> AttrSpec -> Filter RegexStr -> Bool
matchAttrFilter dn (a, _) f = case f of
    FilterAttr atf -> not $ a =~ atf
    FilterAttrDn dnf atf -> not $ dn =~ dnf && a =~ atf
    _ -> True

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
                    then (a, filter (not . (=~ valf)) v)
                    else e
        applyFilter acc _ = acc

isDnFilter :: Filter RegexStr -> Bool
isDnFilter (FilterDn _) = True
isDnFilter _ = False

