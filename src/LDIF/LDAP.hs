{-| Helpers for working with LDAP

    <jonas.juselius@uit.no> 2014
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module LDIF.LDAP (
      ldapToLdif
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
            (dn', LDIFRecord dn' (LDIFAttrs . HM.fromList $ map toat av))
        toat (a, v) = (T.pack a, HS.fromList (map T.pack v))

recordToLdapAdd :: LDIFAttrs T.Text -> [LDAPMod]
recordToLdapAdd la = map f $ HM.toList (toHM la)
    where
        f (a, v) = LDAPMod LdapModAdd (T.unpack a) $ vl v
        vl v = map T.unpack (HS.toList v)

recordToLdapMod :: LDIFAttrs (LDAPModOp, T.Text) -> [LDAPMod]
recordToLdapMod lm = concatMap f $ HM.toList (toHM lm)
    where
        f (a, v) = map (\(m, x) -> LDAPMod m (T.unpack a) x) $ vl v
        vl v = map (\(m, x) -> (m, [T.unpack x])) (HS.toList v)

