--
-- <jonas.juselius@uit.no> 2014
--
module LDIF.Diff (
      diffLDIF
) where

import LDIF.Types
import Data.Maybe
import Control.Arrow (second)
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
diffEntries l1 l2 = M.unions [adds, deletes, changes]
    where
        adds = l2 `M.difference` l1
        deletes = l1 `M.difference` (l2 `M.difference` adds)
        changes = M.map (uncurry diffRecords . pairup l1') l2'
        l1' = (l1 `M.difference` adds) `M.difference` deletes
        l2' = (l2 `M.difference` adds) `M.difference` deletes
        pairup a b = (fromJust $ M.lookup (rDn b) a, b)

diffRecords :: LDIFRecord -> LDIFRecord -> LDIFRecord
diffRecords (LDIFEntry dn r1) (LDIFEntry _ r2) =
    LDIFChange dn $ M.unions [adds, deletes, changes]
    where
        adds = setOp LdapModAdd $ r2 `M.difference` r1
        deletes = setOp LdapModDelete $
            r1 `M.difference` (r2 `M.difference` adds)
        changes = diffValues r1' r2'
        r1' = (r1' `M.difference` adds) `M.difference` deletes
        r2' = (r2' `M.difference` adds) `M.difference` deletes
        setOp op = M.map (S.map ((,) op))

diffValues :: LDIFRecord -> LDIFRecord -> LDIFRecord
diffValues v1 v2 = S.unions $ [adds, deletes]
    where
        adds = map a2add $ filter (not . isEntryIn r1) r2
        a2add (a, v) = LDAPMod LdapModAdd a v
        (changes, deletes) = foldr processEntry ([], []) r1
        processEntry (a, v) (cx, dx) = maybe delAttr chAttr (lookup a r2)
            where
                delAttr   = (cx, LDAPMod LdapModDelete a v:dx)
                chAttr v' =  (add' ++ cx, del' ++ dx)
                    where
                        (adds', dels) = diffValues v v'
                        add' = map (\x -> LDAPMod LdapModAdd a [x]) adds'
                        del' = map (\x -> LDAPMod LdapModDelete a [x]) dels

{-diffValues :: [Value] -> [Value] -> ([Value], [Value])-}
{-diffValues r1 r2 = (sift r1 r2, sift r2 r1)-}

sift :: Eq a => [a] -> [a] -> [a]
sift x = filter (not . flip elem x)

isEntryIn :: Eq a => [(a, b)] -> (a, b) -> Bool
isEntryIn l (e, _) = isJust $ lookup e l

