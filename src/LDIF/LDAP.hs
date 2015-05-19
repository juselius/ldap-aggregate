{-| Helpers for working with LDAP

    <jonas.juselius@uit.no> 2014
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module LDIF.LDAP (
      ldapToLdif
    , ldifToMod
    , recordToLdapAdd
    , recordToLdapMod
) where

import LDIF.Types
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T

ldapToLdif :: [LDAPEntry] -> LDIFEntries
ldapToLdif x = HM.fromList $ map toll x
    where
        toll (LDAPEntry dn av) = let dn' = T.pack dn in
            (dn', LDIFRecord dn' (HM.fromList $ map toat av))
        toat (a, v) = (T.pack a, HS.fromList (map T.pack v))

ldifToMod :: LDIF -> LDIF
ldifToMod l@LDIF{..} = l {lMod = HM.foldlWithKey' rec2add lMod lRec}
    where
        rec2add acc k (toAdd -> v) = HM.insert k v acc
        toAdd (LDIFRecord dn attrs) = LDIFAdd dn attrs

recordToLdapAdd :: LDIFAttrs T.Text -> [LDAPMod]
recordToLdapAdd la = map f $ HM.toList la
    where
        f (a, v) = LDAPMod LdapModAdd (T.unpack a) $ vl v
        vl v = map T.unpack (HS.toList v)

recordToLdapMod :: LDIFAttrs (LDAPModOp, T.Text) -> [LDAPMod]
recordToLdapMod lm = concatMap f $ HM.toList lm
    where
        f (a, v) = map (\(m, x) -> LDAPMod m (T.unpack a) x) $ vl v
        vl v = map (\(m, x) -> (m, [T.unpack x])) (HS.toList v)

