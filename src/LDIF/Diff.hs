--
-- <jonas.juselius@uit.no> 2014
--
module LDIF.Diff (
      diffLDIF
) where

import LDIF.Types
import Data.Maybe
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S

-- | Calculate Change LDIF between two LDIF contents.
-- If there is not difference the empty change list is returned.
diffLDIF :: LDIF -> LDIF -> LDIF
diffLDIF l1 l2 = diffEntries l1' l2'
    where
        l1' = M.filter isNotMod l1
        l2' = M.filter isNotMod l2
        isNotMod (LDIFEntry _ _) = True
        isNotMod _ = False

diffEntries :: LDIF -> LDIF -> LDIF
diffEntries l1 l2 = M.unions [adds, mkDelete deletes, changes]
    where
        adds = l2 `M.difference` l1
        deletes = l1 `M.difference` (l2 `M.difference` adds)
        changes = M.mapWithKey (\k v -> diffRecords (fetch k l1') v) l2'
        l1' = (l1 `M.difference` adds) `M.difference` deletes
        l2' = (l2 `M.difference` adds) `M.difference` deletes
        fetch a b = fromJust $ M.lookup a b
        mkDelete = M.map (\(LDIFEntry dn _) -> LDIFDelete dn)

diffRecords :: LDIFRecord -> LDIFRecord -> LDIFRecord
diffRecords (LDIFEntry dn r1) (LDIFEntry _ r2) =
    LDIFChange dn $ M.unions [adds, deletes, changes]
    where
        adds = setLdapOp LdapModAdd $ r2 `M.difference` r1
        deletes = setLdapOp LdapModDelete $
            r1 `M.difference` (r2 `M.difference` adds)
        changes = M.mapWithKey (\k v -> diffValues (fetch k r1') v) r2'
        r1' = (r1' `M.difference` adds) `M.difference` deletes
        r2' = (r2' `M.difference` adds) `M.difference` deletes
        fetch a b = fromJust $ M.lookup a b
diffRecords _ _  = undefined

diffValues :: S.HashSet Value -> S.HashSet Value-> S.HashSet (LDAPModOp, Value)
diffValues v1 v2 = S.unions [
    S.map ((,) LdapModAdd) adds,
    S.map ((,) LdapModDelete) deletes]
    where
        adds = v2 `S.difference` v1
        deletes = v2 `S.difference` (v2 `S.difference` adds)

setLdapOp :: LDAPModOp -> Attrs Value -> Attrs (LDAPModOp,Value)
setLdapOp op = M.map (S.map ((,) op))


