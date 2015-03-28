{-| Helpers for working with Simple LDIF

    <jonas.juselius@uit.no> 2014
-}
{-# LANGUAGE RankNTypes #-}

module SimpleLDIF.Utils (
      mapLDIF
    , mapLdif
    , liftLdif
    , showLdif
    , ldapToLdif
    , makeLdifEntry
    , makeLdifChange
) where

import SimpleLDIF.Types
import Control.Arrow (second)
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S

mapLDIF :: (forall a. Attribute -> ValueSet a -> ValueSet a) -> LDIF -> LDIF
mapLDIF f l = M.map (mapLdif f) l

mapLdif :: (forall a. Attribute -> ValueSet a -> ValueSet a)
        -> LDIFRecord
        -> LDIFRecord
mapLdif f l =
    case l of
        LDIFAdd  _ av -> l { rAttrs = M.mapWithKey f av }
        LDIFChange _ av -> l { rMods  = M.mapWithKey f av }
        LDIFDelete _    -> l
        --where
            --applyf  k v = S.map (f k) v
            --applyf' k v = S.map (second (f k)) v

liftLdif :: (DN -> DN) -> LDIFRecord -> LDIFRecord
liftLdif f l = l { rDn = f $ rDn l }

showLdif :: LDIF -> String
showLdif l = unwords . map show $ M.elems l

ldapToLdif :: [LDAPEntry] -> LDIF
ldapToLdif x = M.fromList $ map toll x
    where
        toll (LDAPEntry dn av) =
            (dn, LDIFAdd dn (M.fromList $ map toat av))
        toat (a, v) =
            (a, S.fromList v)

makeLdifEntry :: DN -> [AttrSpec]-> LDIFRecord
makeLdifEntry dn av =
    LDIFAdd dn (M.fromList $ map (second S.fromList) av)

makeLdifChange :: DN -> LDAPModOp -> Attribute -> [Value]-> LDIFRecord
makeLdifChange dn op a v =
    LDIFChange dn (M.singleton a (S.fromList (zip (repeat op) v)))

