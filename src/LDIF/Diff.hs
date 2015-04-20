--
-- <jonas.juselius@uit.no> 2014
--
module LDIF.Diff (
      diffLDIF
) where

import LDIF.Types
import Data.Maybe
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

-- | Calculate Change LDIF between two LDIF contents.
-- If there is not difference the empty change list is returned.
diffLDIF :: LDIFEntries -> LDIFEntries -> LDIFMods
diffLDIF l1 l2 =
    HM.unions [toAdd adds, toDelete deletes, changes]
    where
        adds = l2 `HM.difference` l1
        deletes = l1 `HM.difference` (l2 `HM.difference` adds)
        changes = HM.mapWithKey (\k v ->
          diffRecords (fetch k l1') v) l2'
        l1' = (l1 `HM.difference` adds) `HM.difference` deletes
        l2' = (l2 `HM.difference` adds) `HM.difference` deletes
        fetch a b = fromJust $ HM.lookup a b
        toAdd    = HM.map (\(LDIFRecord dn x) -> LDIFAdd dn x)
        toDelete = HM.map (\(LDIFRecord dn _) -> LDIFDelete dn)

diffRecords :: LDIFRecord -> LDIFRecord -> LDIFMod
diffRecords (LDIFRecord dn r1) (LDIFRecord _ r2) =
    LDIFChange dn $ HM.unions [adds, deletes, changes]
    where
        adds = setLdapOp LdapModAdd $ r2 `HM.difference` r1
        deletes = setLdapOp LdapModDelete $
            r1 `HM.difference` (r2 `HM.difference` adds)
        changes = HM.mapWithKey (\k v ->
            diffValues (fetch k r1') v) r2'
        r1' = (r1 `HM.difference` adds) `HM.difference` deletes
        r2' = (r2 `HM.difference` adds) `HM.difference` deletes
        fetch a b = fromJust $ HM.lookup a b

diffValues :: HS.HashSet Value
           -> HS.HashSet Value
           -> HS.HashSet (LDAPModOp, Value)
diffValues v1 v2 =
    HS.unions [
          HS.map ((,) LdapModAdd) adds
        , HS.map ((,) LdapModDelete) deletes
        ]
    where
        adds = v2 `HS.difference` v1
        deletes = v1 `HS.difference` v2
        -- deletes = v2 `HS.difference` (v2 `HS.difference` adds)

setLdapOp :: LDAPModOp
          -> HM.HashMap DN (HS.HashSet Value)
          -> HM.HashMap DN (HS.HashSet (LDAPModOp, Value))
setLdapOp op = HM.map (HS.map ((,) op))


