{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module TestData where

import LDIF
import Control.Arrow (first, second)
import Test.QuickCheck
import Control.Applicative
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

type AttrList = [(T.Text, [T.Text])]

genLdif' :: DN -> AttrList -> T.Text
genLdif' dn av = "dn: " `T.append` dn `T.append` "\n" `T.append` attrs
    where
        attrs = foldl (\s (a, v) ->
            s    ` T.append `
            a    ` T.append `
            ": " ` T.append `
            v    ` T.append `
            "\n") "" (expanded av)
        expanded = concatMap (uncurry zip . first repeat)

genLdif :: DN -> AttrList -> LDIF
genLdif dn av = HM.singleton dn $ makeLdifEntry dn av

makeLdifEntry :: DN -> [(LdifAttr, [LdifValue])]-> LDIFRecord
makeLdifEntry dn av =
    LDIFAdd dn (HM.fromList $ map (second HS.fromList) av)

makeLdifChange :: DN -> LDAPModOp -> LdifAttr -> [LdifValue]-> LDIFRecord
makeLdifChange dn op a v =
    LDIFChange dn (HM.singleton a (HS.fromList (zip (repeat op) v)))

newtype LdifStr = LdifStr { ldifStr :: T.Text } deriving (Show)
newtype LdifEntryStr = LdifEntryStr { entryStr :: T.Text } deriving (Show)

instance Arbitrary LdifStr where
    arbitrary = do
        dn <- genDN
        ch <- genChangeType
        attrs <- genAttrs dn ch
        return $ LdifStr $ stringify [dn, ch, attrs]
        where
            stringify = T.init . T.unlines . filter (not . T.null)

instance Arbitrary LdifEntryStr where
    arbitrary = do
        dn <- genDN
        attrs <- genAttrs dn ""
        return $ LdifEntryStr $ stringify [dn, attrs]
        where
            stringify = T.init . T.unlines . filter (not . T.null)

genDN :: Gen T.Text
genDN = do
    cn <- T.append <$> pure "cn=" <*> genSafeString1
    ou <- listOf $ T.append <$> pure "ou=" <*> genSafeString1
    dc <- listOf $ T.append <$> pure "dc=" <*> genSafeString1
    return $ "dn: " `T.append` T.intercalate "," (filter (not . T.null) [
        cn, T.intercalate "," ou, T.intercalate "," dc
        ])

genAttrs :: T.Text -> T.Text -> Gen T.Text
genAttrs dn ch
    | "delete" `T.isInfixOf` ch = return ""
    | "modify" `T.isInfixOf` ch = do
        a <- listOf1 genModAttr
        return $ T.unlines a
    | otherwise = do
        a <- listOf genAttr
        return $ cn `T.append` T.unlines a
    where
        cn = "cn: "
            `T.append` (T.drop 7 . T.takeWhile (/= ',') $ dn)
            `T.append` "\n"

genModAttr :: Gen T.Text
genModAttr = do
    op <- elements ["add: ", "delete: ", "replace: "]
    a <- genSafeString1
    v <- genSafeString1
    return $ op
        `T.append` a
        `T.append` "\n"
        `T.append` a
        `T.append` ": "
        `T.append` v
        `T.append` "\n-"

genAttr :: Gen T.Text
genAttr = do
    a <- genSafeString1
    v <- genSafeString1
    return $ a `T.append` ": " `T.append` v

genChangeType :: Gen T.Text
genChangeType = do
    ch <- elements ["add", "delete\n", "modify"]
    return $ if not (T.null ch)
        then "changetype: " `T.append` ch
        else ""

plainChar :: String
plainChar = ['a'..'z'] ++ ['A'..'Z'] ++ "åäöÅÄÖæøÆØ"

genSafeChar :: Gen Char
genSafeChar = elements plainChar

genSafeAlpha :: Gen Char
genSafeAlpha = elements $ plainChar ++ "0123456789"

genSafeString :: Gen T.Text
genSafeString = liftA T.pack $ (:) <$> genSafeChar <*> listOf genSafeAlpha

genSafeString1 :: Gen T.Text
genSafeString1 = liftA T.pack $ (:) <$> genSafeChar <*> listOf1 genSafeAlpha


ldifAttrs1 :: AttrList
ldifAttrs1 =
    [ ("A", ["A1", "A2"])
    , ("B", ["B1"])
    , ("C", ["C1"])
    ]

ldifAttrs2 :: AttrList
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

