{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module TestData where

import SimpleLDIF
import Control.Arrow (first)
import Test.QuickCheck
import Control.Applicative
import Data.List
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM

genLdif' :: DN -> [AttrSpec] -> T.Text
genLdif' dn av = T.pack $ "dn: " ++ dn ++ "\n" ++ attrs
    where
        attrs = foldl (\s (a, v) ->
            s ++ a ++ ": " ++ v ++ "\n") "" (expanded av)
        expanded = concatMap (uncurry zip . first repeat)

genLdif :: DN -> [AttrSpec] -> LDIF
genLdif dn av = HM.singleton dn $ makeLdifEntry dn av

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
    ch <- elements ["add", "delete\n", "modify"]
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


ldifAttrs1 :: [AttrSpec]
ldifAttrs1 =
    [ ("A", ["A1", "A2"])
    , ("B", ["B1"])
    , ("C", ["C1"])
    ]

ldifAttrs2 :: [AttrSpec]
ldifAttrs2 =
    [ ("A", ["A1"])
    , ("B", ["B2"])
    , ("D", ["D1"])
    ]

[testLdif1, testLdif2] = map (uncurry genLdif) [l1, l2]
    where
        l1 = ("dc=foo,dc=com", ldifAttrs1)
        l2 = ("dc=foo,dc=com", ldifAttrs2)

[testLdif1', testLdif2'] = map (uncurry genLdif) [l1, l2]
    where
        l1 = ("dc=bar,dc=org", ldifAttrs1)
        l2 = ("dc=bar,dc=org", ldifAttrs2)

sampleLdif = sample' (arbitrary :: Gen LdifStr)

