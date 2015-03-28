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

{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module SimpleLDIF.Preproc (
     preproc
   , transposePos
   , PosTable
) where
import Text.Parsec
import Text.Parsec.Error (setErrorPos)
import qualified Data.ByteString.Char8 as BC
import Data.List (foldl', sortBy)

-- | Opaque data necessary for relation between text after preprocessing and original
type PosTable = [ PosOp ]

data PosOp = PosOpAddLine { psLine :: Int }
           | PosOpWrap    { psLine :: Int, psW :: Int, psWP :: Int } deriving Show

data LdifLine = LdifLine     { llNum :: Int, llStr :: BC.ByteString }
              | LdifLineCont { llNum :: Int, llStr :: BC.ByteString }
              | LdifComment  { llNum :: Int, llStr :: BC.ByteString }

-- | Convert error position to original text before preprocessing
transposePos :: PosTable -> ParseError -> ParseError
transposePos ptab oe = setErrorPos npos oe
  where
    opos = errorPos oe
    npos = setSourceColumn (setSourceLine opos nlin) ncol
      where
        opIdx a b = (psLine a) `compare` (psLine b)
        ocord = (sourceLine opos,sourceColumn opos)
        (nlin,ncol) = calcPos (sortBy opIdx ptab) ocord

calcPos :: PosTable -> (Int, Int) -> (Int, Int)
calcPos xs cord = foldl' updatePos cord xs
  where
    updatePos (l0,c0) (PosOpAddLine l)    | l0 >= l        = (l0+1,c0)
    updatePos (l0,c0) (PosOpWrap l _ _)  | l0 >= l        = (l0+1,c0)
    updatePos (l0,c0) (PosOpWrap l w wp)  | (l0+1) == l && c0 > w && (c0-1-w) > wp  = (l0+1,c0)
    updatePos (l0,c0) (PosOpWrap l w wp)  | (l0+1) == l && c0 > w && (c0-1-w) <= wp = (l0+1,c0-w)
    updatePos x _ = x

-- | Preprocessing of LDIF file, concat wrapped lines and remove comment lines
preproc :: BC.ByteString -> (BC.ByteString, PosTable)
preproc xs = (str, ptab)
  where
    str = BC.unlines $ map llStr ys
    (ys, ptab) = lns xs
      where
        lns zs = stripComments $ unwrap $ (tokenizeLines $ specLines zs, [])

specLines :: BC.ByteString -> [BC.ByteString]
specLines xs = map cleanLine $ BC.lines xs
  where
    isCR c = c == '\r'
    cleanLine l | BC.null l        = l
                | isCR (BC.last l) = cleanLine $ BC.init l
                | otherwise        = l

tokenizeLines :: [BC.ByteString] -> [LdifLine]
tokenizeLines xs = map tokenizeLine $ zip xs [1..]
  where
    tokenizeLine (x,i) | BC.null x        = LdifLine     i BC.empty
                       | BC.head x == '#' = LdifComment  i x
                       | BC.head x == ' ' = LdifLineCont i $ BC.tail x
                       | otherwise        = LdifLine     i x

-- | Remove Comment Lines
stripComments :: ([LdifLine],PosTable) -> ([LdifLine],PosTable)
stripComments (xs,pt) = foldl' procLine ([],pt) xs
  where
    procLine (v,p) (LdifComment i _) = (v,(PosOpAddLine i):p)
    procLine (v,p) o                 = (o:v,p)

-- | Unwrap lines, lines with space at begin is continue of previous line
unwrap :: ([LdifLine],PosTable) -> ([LdifLine],PosTable)
unwrap (xs,pt) = foldl' procLine ([],pt) xs
  where
    procLine ([],p) o = (o:[],p)
    procLine (v,p) (LdifLineCont i s) = let (z,r) = splitAt 1 v
                                            o = head z
                                            o' = o { llStr = (llStr o) `BC.append` s }
                                            p' = (PosOpWrap i (BC.length $ llStr o) (BC.length s)):p
                                        in (o':r,p')
    procLine (v,p) o                  = (o:v,p)
