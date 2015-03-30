{-| Helpers for working with Simple LDIF

    <jonas.juselius@uit.no> 2014
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module SimpleLDIF.Utils (
      mapLDIF
    , mapLdif
    , liftLdif
    , showLdif
    , ldapToLdif
    , recordToLdapAdd
    , recordToLdapMod
    , makeLdifEntry
    , makeLdifChange
) where

import SimpleLDIF.Types
import Control.Arrow (second)
import Data.List (concat)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T

mapLDIF :: (forall a. LdifAttr -> LdifValues a -> LdifValues a) -> LDIF -> LDIF
mapLDIF f = HM.map (mapLdif f)

mapLdif :: (forall a. LdifAttr -> LdifValues a -> LdifValues a)
        -> LDIFRecord
        -> LDIFRecord
mapLdif f l =
    case l of
        LDIFAdd  _ av -> l { rAttrs = HM.mapWithKey f av }
        LDIFChange _ av -> l { rMods  = HM.mapWithKey f av }
        LDIFDelete _    -> l

liftLdif :: (DN -> DN) -> LDIFRecord -> LDIFRecord
liftLdif f l = l { rDn = f $ rDn l }

showLdif :: LDIF -> T.Text
showLdif l = T.unwords . map (T.append "\n---\n" . T.pack . show) $ HM.elems l

ldapToLdif :: [LDAPEntry] -> LDIF
ldapToLdif x = HM.fromList $ map toll x
    where
        toll (LDAPEntry dn av) = let dn' = T.pack dn in
            (dn', LDIFAdd dn' (HM.fromList $ map toat av))
        toat (a, v) =
            (T.pack a, HS.fromList (map T.pack v))

recordToLdapAdd :: LdifAttrs T.Text -> [LDAPMod]
recordToLdapAdd la = map f $ HM.toList la
    where
        f (a, v) = LDAPMod LdapModAdd (T.unpack a) $ vl v
        vl v = map T.unpack (HS.toList v)

recordToLdapMod :: LdifAttrs (LDAPModOp, T.Text) -> [LDAPMod]
recordToLdapMod lm = concat . map f $ HM.toList lm
    where
        f (a, v) = map (\(m, x) -> LDAPMod m (T.unpack a) x) $ vl v
        vl v = map (\(m, x) -> (m, [T.unpack x])) (HS.toList v)

makeLdifEntry :: DN -> [(LdifAttr, [LdifValue])]-> LDIFRecord
makeLdifEntry dn av =
    LDIFAdd dn (HM.fromList $ map (second HS.fromList) av)

makeLdifChange :: DN -> LDAPModOp -> LdifAttr -> [LdifValue]-> LDIFRecord
makeLdifChange dn op a v =
    LDIFChange dn (HM.singleton a (HS.fromList (zip (repeat op) v)))

