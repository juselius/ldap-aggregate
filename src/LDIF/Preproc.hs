{-# LANGUAGE OverloadedStrings #-}

module LDIF.Preproc (
      preproc
    )
where
import qualified Data.ByteString.Char8 as BC
import Data.List (foldl')

data LdifLine = LdifLine     { lstr :: BC.ByteString }
              | LdifLineCont { lstr :: BC.ByteString }
              | LdifComment  { lstr :: BC.ByteString }

-- | Preprocessing of LDIF file, concat wrapped lines and remove comment lines
preproc :: BC.ByteString -> BC.ByteString
preproc xs = BC.unlines $ map lstr (lns xs)
    where
        lns x = stripComments . unwrap . tokenizeLines $ specLines x

specLines :: BC.ByteString -> [BC.ByteString]
specLines xs = map cleanLine $ BC.lines xs
    where
        isCR c = c == '\r'
        cleanLine l | BC.null l        = l
                    | isCR (BC.last l) = cleanLine $ BC.init l
                    | otherwise        = l

tokenizeLines :: [BC.ByteString] -> [LdifLine]
tokenizeLines xs = map tokenizeLine xs
    where
        tokenizeLine x | BC.null x        = LdifLine BC.empty
                       | BC.head x == '#' = LdifComment x
                       | BC.head x == ' ' = LdifLineCont (BC.tail x)
                       | otherwise        = LdifLine     x

-- | Remove Comment Lines
stripComments :: [LdifLine] -> [LdifLine]
stripComments xs = foldl' procLine [] xs
    where
        procLine v (LdifComment _) = v
        procLine v o = o:v

-- | Unwrap lines, lines with space at begin is continue of previous line
unwrap :: [LdifLine] -> [LdifLine]
unwrap xs = foldl' procLine [] xs
    where
        procLine [] o = o:[]
        procLine v (LdifLineCont s) = let
            (z,r) = splitAt 1 v
            o = head z
            o' = o { lstr = (lstr o) `BC.append` s } in
                o':r
        procLine v o = o:v
