--
-- <jonas.juselius@uit.no> 2014
--
{-# LANGUAGE LambdaCase #-}

module LDAPAggregate.Filter (
      filterLdif
    --, filterDn
    --, filterEntries
    --, mkFilterAction
) where

import SimpleLDIF
import LDAPAggregate.Types
import Text.Regex.Posix
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S

--mkFilterAction :: [Regexp] -> Regexp -> Action
--mkFilterAction [dn] r = Ignore [CullDn dn] (=~ r)
--mkFilterAction [dn, a] r = Ignore [CullDn dn, CullAttr a] (=~ r)
--mkFilterAction [dn, a, v] r = Ignore [CullDn dn, CullAttr a, CullVal v] (=~ r)

filterLdif:: [Culler] -> LDIF -> LDIF
filterLdif css = M.foldl' (filterRecord css) M.empty

filterRecord :: [Culler] -> LDIF -> LDIFRecord -> LDIF
filterRecord css ldif rec =
    if any matchDn css'
    then ldif
    else if not $ M.null $ M.foldlWithKey' (filterFields css') M.empty rec
        then M.insert dn rec ldif
        else ldif
    where
        css' = filter (flip cullDn dn) css
        dn = rDn rec
        matchDn x@(CullDn _) = cullDn x dn
        matchDn _ = False

filterFields :: [Culler] -> LDIFRecord -> String -> ValueSet a -> LDIFRecord
filterFields css rec k v = rec

--filterLdifRecord :: [Filter] -> LDIFRecord -> LDIFRecord
--filterLdifRecord fs r  =
    --case r of
        --LDIFEntry  {} -> r { rAttrs = applyFilters filterValues (rAttrs r) }
        --LDIFChange {} -> r { rMods  = applyFilters filterValues (rMods r) }
        --LDIFDelete {} -> r
    --where
        --applyFilters g r = M.filter (S.null) $ M.mapWithKey (g vfs) (pertain r)
        --pertain = M.filterWithKey (dropAttr afs)
        --fs' = filter (isActiveFilter (rDn r)) fs
        --afs = filter isAttrFilter fs'
        --vfs = filter isValueFilter fs'

--dropAttr :: [Filter] -> Attribute -> a -> Bool
--dropAttr fs attr _ = all $ map isMatch fs
    --where
        --isMatch = \case
            --FilterAttr _ atf -> not $ attr =~ atf
            --_ -> True

--filterValues :: [Filter] -> Attribute -> ValueSet a -> ValueSet a
--filterValues f a v = S.filter applyFilters v
    --where
        --applyFilters x = all notMatching f'
        --f' = filter pertain f
        --pertain (FilterVal _ atf _) = a =~ atf
        --pertain _ = False
        --notMatching (FilterVal _ _ vf) val = not $ v =~ vf
        --notMatching  _ = True

{-filterValues' :: [Filter] -> Attribute -> ValueSet (LDAPModOp, String) -> ValueSet (LDAPModOp, String)-}
{-filterValues' f a v = S.filter runFilters v-}
    {-where-}
        {-f' = filter pertains f-}
        {-pertains (FilterVal _ atf _) = a =~ atf-}
        {-pertains _ = False-}
        {-runFilters x = all (\(FilterVal _ _ vf) -> x =~ vf) f'-}

--isDnFilter :: Filter -> Bool
--isDnFilter = \case
    --FilterRecord _ -> True
    --_ -> False

--isAttrFilter :: Filter -> Bool
--isAttrFilter = \case
    --FilterAttr   {} -> True
    --_ -> False

--isValueFilter :: Filter -> Bool
--isValueFilter = \case
    --FilterVal   {} -> True
    --_ -> False

--isActiveFilter :: DN -> Filter -> Bool
--isActiveFilter dn = \case
    --FilterAttr dnf _   -> dn =~ dnf
    --FilterVal  dnf _ _ -> dn =~ dnf
    --_ -> True

