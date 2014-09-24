-- Copyright (c) 2011, Radoslav Dorick <radoslav.dorick@gmail.com>
--
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 1. Redistributions of source code must retain the above copyright notice,
-- this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
-- notice, this list of conditions and the following disclaimer in the
-- documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the copyright holder nor the names of its
-- contributors may be used to endorse or promote products derived from this
-- software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
-- CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT
-- NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
-- PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER
-- OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
-- EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
-- PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
-- OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
-- WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
-- OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
-- ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--
-- Modifications by Jonas Juselius <jonas.juselius@uit.no>
--
module LDIF.Diff (
      diffLDIF
    , diffRecord
) where

import LDIF.Types
import LDAP.Modify (list2ldm)
import Data.Maybe
import Control.Arrow (second)

-- | Calculate difference between two LDAP entries
diffRecord :: LDIFRecord -> LDIFRecord -> LDIFRecord
diffRecord r1 r2 = diffAttrs (getEntry r1) (getEntry r2)
    where
        getEntry = leattrs . fromJust . record2entry []

-- | Calculate Change LDIF between two LDIF contents.
-- If there is not difference the empty change list is returned.
diffLDIF :: [LDAPEntry] -> [LDAPEntry] -> [LDIF]
diffLDIF r1 r2 = adds ++ deletes ++ changes
    where
        adds = map e2add $ filter (not . isEntryIn l1) l2
        (changes, deletes) = foldr processEntry ([], []) l1
        [l1, l2] = map (map (\(LDAPEntry dn av) -> (dn, av))) [r1, r2]
        e2add x = LDIF $ second (LDIFAdd . list2ldm LdapModAdd) x
        processEntry (dn, e1) (cx, dx) = maybe delRec chRec (lookup dn l2)
            where
                chRec e2 = (diffToLdif e2:cx, dx)
                delRec = (cx, LDIF (dn, LDIFDelete):dx)
                diffToLdif e2 = LDIF (dn, diffAttrs e1 e2)

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

--testAV1 = [("foo", ["foo1", "bar1"]), ("bar", ["foo2", "bar2"])]
--testAV2 = [("foo2", ["foo2", "bar2"])]
