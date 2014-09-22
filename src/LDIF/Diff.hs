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
import Data.List (foldl')
import Control.Arrow (second)

-- | Calculate Change LDIF between two LDIF contents.
-- If there is not difference the empty change list is returned.
diffLDIF :: [LDIFContents] -> [LDIFContents] -> [LDIF]
diffLDIF c1 c2 = adds  ++ deletes ++ changes
    where
        adds = map content2add $ filter (not . isEntryIn) l2
            where
                isEntryIn (dn, _) = maybe False (const True) (lookup dn l1)
                content2add x = LDIF $ second LDIFAdd x
        (changes, deletes) = foldl' processEntry ([], []) l1
        processEntry (cx, dx) (dn, e1) = maybe delRec chRec (lookup dn l2)
            where
                chRec e2 = (diffToLdif e2 ++ cx, dx)
                delRec = (cx, (LDIF (dn, LDIFDelete)):dx)
                diffToLdif e2 = map (\x -> LDIF (dn, x)) $ diffAttrs e1 e2
        l1 = toAList c1
        l2 = toAList c2

-- | Calculate difference between two LDIF Records
diffRecord :: LDIFRecord -> LDIFRecord -> [LDIFRecord]
diffRecord LDIFDelete _ = []
diffRecord _ LDIFDelete = []
diffRecord r1 r2 = diffAttrs (ldifRecord r1) (ldifRecord r2)

diffAttrs :: [AttrSpec] -> [AttrSpec] -> [LDIFRecord]
diffAttrs r1 r2 = delMods ++ addMods
    where
        addMods = map LDIFAdd addVals
        delMods = map (LDIFChange LdapModDelete) delVals
        addVals = []
        delVals = []
        --addVals = filter (\x -> not $ elem x (coAttrVals r1)) (coAttrVals r2) :: [AttrValue]
        --delVals = filter (\x -> not $ elem x (coAttrVals r2)) (coAttrVals r1) :: [AttrValue]
