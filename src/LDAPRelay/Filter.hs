--
-- <jonas.juselius@uit.no> 2014
--

module LDAPRelay.Filter (
      filterLdif
    , filterDN
    , filterEntries
    , makeDnFilters
    , makeAttrFilters
) where

import LDIF
import LDAPRelay.Types
import Text.Regex.Posix
import Control.Arrow (second)
import Data.List (partition, foldl')

makeDnFilters :: [String] -> [Filter]
makeDnFilters = map FilterDn

makeAttrFilters :: [[String]] -> [Filter]
makeAttrFilters = map toFilter
    where
        toFilter (a:[]) = FilterAttr a
        toFilter (dn:a:[]) = FilterDnAttr dn a
        toFilter (dn:a:v:[]) = FilterDnVal dn a v
        toFilter x = error $ "Invalid filter spec: " ++ show x


filterLdif :: [Filter] -> [LDIF] -> [LDIF]
filterLdif fs ldif = filterEntries fs ldif'
    where
        ldif' = filterDN fs' ldif
        fs' = fst $ partition isDnFilter fs

filterDN :: [Filter] -> [LDIF] -> [LDIF]
filterDN fs = filter (\(dn, _) -> any (dnFilter dn) fs)
    where
        dnFilter dn' (FilterDn f) = f =~ dn'
        dnFilter _ _ = False

filterEntries :: [Filter] -> [LDIF] -> [LDIF]
filterEntries fs = map (filterLDIF fs')
    where
        fs' = snd $ partition isDnFilter fs

filterLDIF :: [Filter] -> LDIF -> LDIF
filterLDIF fs l@(dn, _) = second (liftLdif (runAttrFilters dn fs)) l

runAttrFilters :: DN -> [Filter] -> [AttrSpec] -> [AttrSpec]
runAttrFilters dn fs av = filter (\x -> any (matchAvFilter dn x) fs') av'
    where
        av' = filter (null . snd) $ map (filterValues dn fs'') av
        (fs'', fs') = partition isValueFilter fs
        isValueFilter (FilterDnVal {}) = True
        isValueFilter _ = False

matchAvFilter :: DN -> AttrSpec -> Filter -> Bool
matchAvFilter dn (a, _) f = case f of
    FilterAttr atf -> a =~ atf
    FilterDnAttr dnf atf -> dn =~ dnf && a =~ atf
    _ -> False

filterValues :: DN -> [Filter] -> AttrSpec -> AttrSpec
filterValues dn f as = foldl' applyFilter as f
    where
        applyFilter av (FilterDnVal dnf atf valf) =
            if dn =~ dnf
            then filterAV av
            else av
            where
                filterAV e@(a, v) =
                    if a =~ atf
                    then (a, filter (=~ valf) v)
                    else e
        applyFilter acc _ = acc

isDnFilter :: Filter -> Bool
isDnFilter (FilterDn _) = True
isDnFilter _ = False

