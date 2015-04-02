{-| Helpers for working with Simple LDIF

    <jonas.juselius@uit.no> 2014
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module SimpleLDIF.Utils (
      showLdif
    , ldapToLdif
    , recordToLdapAdd
    , recordToLdapMod
    , makeLdifEntry
    , makeLdifChange
) where

import SimpleLDIF.Types
import Control.Arrow (second)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T

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
recordToLdapMod lm = concatMap f $ HM.toList lm
    where
        f (a, v) = map (\(m, x) -> LDAPMod m (T.unpack a) x) $ vl v
        vl v = map (\(m, x) -> (m, [T.unpack x])) (HS.toList v)

makeLdifEntry :: DN -> [(LdifAttr, [LdifValue])]-> LDIFRecord
makeLdifEntry dn av =
    LDIFAdd dn (HM.fromList $ map (second HS.fromList) av)

makeLdifChange :: DN -> LDAPModOp -> LdifAttr -> [LdifValue]-> LDIFRecord
makeLdifChange dn op a v =
    LDIFChange dn (HM.singleton a (HS.fromList (zip (repeat op) v)))

