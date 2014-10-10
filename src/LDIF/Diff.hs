--
-- <jonas.juselius@uit.no> 2014
--
module LDIF.Diff (
      diffLDIF
    , diffRecord
    , diffAttrs
) where

import LDIF.Types
import LDIF.Utils
import LDAP.Modify (list2ldm)
import Data.Maybe
import Control.Arrow (second)

-- | Calculate difference between two LDAP entries
diffRecord :: LDIFRecord -> LDIFRecord -> LDIFRecord
diffRecord r1 r2 = diffAttrs (getEntry r1) (getEntry r2)
    where
        getEntry = leattrs . fromJust . ldifRecord2Entry []

-- | Calculate Change LDIF between two LDIF contents.
-- If there is not difference the empty change list is returned.
diffLDIF :: [LDIF] -> [LDIF] -> [LDIF]
diffLDIF r1 r2 = diffLDAP x1 x2
    where
        (x1, x2) = bimap1 (map toEntry) (r1, r2)
        toEntry (dn, e) = fromJust $ ldifRecord2Entry dn e

diffLDAP :: [LDAPEntry] -> [LDAPEntry] -> [LDIF]
diffLDAP r1 r2 = adds ++ deletes ++ changes
    where
        adds = map e2add $ filter (not . isEntryIn l1) l2
        (changes, deletes) = foldr processEntry ([], []) l1
        [l1, l2] = map (map (\(LDAPEntry dn av) -> (dn, av))) [r1, r2]
        e2add = second (LDIFAdd . list2ldm LdapModAdd)
        processEntry (dn, e1) (cx, dx) = maybe delRec chRec (lookup dn l2)
            where
                chRec e2 = (diffToLdif e2:cx, dx)
                delRec = (cx, (dn, LDIFDelete):dx)
                diffToLdif e2 = (dn, diffAttrs e1 e2)

diffAttrs :: [AttrSpec] -> [AttrSpec] -> LDIFRecord
diffAttrs r1 r2 = LDIFChange $ adds ++ deletes ++ changes
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

diffValues :: [Value] -> [Value] -> ([Value], [Value])
diffValues r1 r2 = (sift r1 r2, sift r2 r1)

sift :: Eq a => [a] -> [a] -> [a]
sift x = filter (not . flip elem x)

isEntryIn :: Eq a => [(a, b)] -> (a, b) -> Bool
isEntryIn l (e, _) = isJust $ lookup e l

