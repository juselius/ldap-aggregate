{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module TestData where

import LDIF
import Control.Arrow (first)
import qualified Data.ByteString.Char8 as BS
import Test.QuickCheck
import Control.Applicative
import Data.List

genLdif' :: String -> [AttrSpec] -> BS.ByteString
genLdif' dn av = BS.pack $ "dn: " ++ dn ++ "\n" ++ attrs
    where
        attrs = foldl (\s (a, v) ->
            s ++ a ++ ": " ++ v ++ "\n") "" (expanded av)
        expanded = concat . map (uncurry zip . first repeat)

genLdif :: String -> [AttrSpec] -> LDIF
genLdif dn av = LDIF (dn, LDIFEntry $ LDAPEntry dn av)

newtype LdifStr = LdifStr { ldifStr :: String } deriving (Show)
newtype LdifEntryStr = LdifEntryStr { entryStr :: String } deriving (Show)

instance Arbitrary LdifStr where
    arbitrary = do
        dn <- genDN
        ch <- genChangeType
        attrs <- genAttrs dn ch
        return $ LdifStr $ stringify [dn, ch, attrs]
        where
            stringify = init . unlines . filter (not . null)

instance Arbitrary LdifEntryStr where
    arbitrary = do
        dn <- genDN
        attrs <- genAttrs dn ""
        return $ LdifEntryStr $ stringify [dn, attrs]
        where
            stringify = init . unlines . filter (not . null)

genDN :: Gen String
genDN = do
    cn <- (++) <$> pure "cn=" <*> genSafeString1
    ou <- listOf $ (++) <$> pure "ou=" <*> genSafeString1
    dc <- listOf $ (++) <$> pure "dc=" <*> genSafeString1
    return $ "dn: " ++ intercalate "," (filter (not . null) [
        cn, intercalate "," ou, intercalate "," dc
        ])

genAttrs :: String -> String -> Gen String
genAttrs dn ch
    | "delete" `isInfixOf` ch = return ""
    | "modify" `isInfixOf` ch = do
        a <- listOf1 genModAttr
        return $ unlines a
    | otherwise = do
        a <- listOf genAttr
        return $ cn ++ unlines a
    where
        cn = "cn: " ++ (drop 7 . takeWhile (/= ',') $ dn) ++ "\n"

genModAttr :: Gen String
genModAttr = do
    op <- elements ["add: ", "delete: ", "replace: "]
    a <- genSafeString1
    v <- genSafeString1
    return $ op ++ a ++ "\n" ++ a ++ ": " ++ v ++ "\n-"

genAttr :: Gen String
genAttr = do
    a <- genSafeString1
    v <- genSafeString1
    return $ a ++ ": " ++ v

genChangeType :: Gen String
genChangeType = do
    ch <- elements ["add", "delete\n", "modify", ""]
    return $ if not (null ch) then "changetype: " ++ ch else ""

plainChar = ['a'..'z'] ++ ['A'..'Z'] ++ "åäöÅÄÖæøÆØ"

genSafeChar :: Gen Char
genSafeChar = elements plainChar

genSafeAlpha :: Gen Char
genSafeAlpha = elements $ plainChar ++ "0123456789"

genSafeString :: Gen String
genSafeString = (:) <$> genSafeChar <*> listOf genSafeAlpha

genSafeString1 :: Gen String
genSafeString1 = (:) <$> genSafeChar <*> listOf1 genSafeAlpha


ldifDef1 :: [AttrSpec]
ldifDef1 = [
      ("A", ["A1", "A2"])
    , ("B", ["B1"])
    ]

ldifDef2 :: [AttrSpec]
ldifDef2 = [
      ("A", ["A1"])
    ]

ldiff1 :: [LDIF]
ldiff1 = [
      LDIF ("B", LDIFAdd [LDAPMod LdapModAdd "A" ["A1"]])
    , LDIF ("A", LDIFDelete)
    ]

ldiff2 :: [LDIF]
ldiff2 = [
      LDIF ("A", LDIFAdd [ LDAPMod LdapModAdd "A" ["A1", "A2"]
                         , LDAPMod LdapModAdd "B" ["B1"]
                         ]
           )
    , LDIF ("B", LDIFDelete)
    ]

sampleLdif = sample' (arbitrary :: Gen LdifStr)

