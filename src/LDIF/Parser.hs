{-| The module emits data structures directly usable by the LDAP package.

    This is a simplified version of Text.LDIF.Parser, originally by
    Radoslav Dorick <radoslav.dorick@gmail.com>. It expexts valid and
    well formed LDIF as input. It does no actual parsing and validation of
    entries.

    <jonas.juselius@uit.no> 2014
-}
{-# LANGUAGE OverloadedStrings, PackageImports #-}

module LDIF.Parser (
      parseLdifStr
    , parseLdifStr'
    , parseLdif
    , parseLdif'
) where

import Prelude
import Data.List
import Control.Monad
import LDIF.Types
import LDIF.Preproc
import Data.Hashable
import "parsec" Text.Parsec as PR
import "parsec" Text.Parsec.ByteString
import Control.Arrow (second)
import qualified Data.ByteString.Char8 as BC
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S

parseLdif :: BC.ByteString -> LDIF
parseLdif ldif = either (error . show) id (parseLdifStr [] ldif)

parseLdif' :: BC.ByteString -> [(DN, LDIFRecord)]
parseLdif' ldif = either (error . show) id (parseLdifStr' [] ldif)

-- | Parse LDIF content
parseLdifStr :: FilePath -> BC.ByteString -> Either ParseError LDIF
parseLdifStr name xs = case parseLdifStr' name xs of
    Left err -> Left err
    Right ldif -> Right $ M.fromList ldif

parseLdifStr' :: FilePath -> BC.ByteString
    -> Either ParseError [(DN, LDIFRecord)]
parseLdifStr' name xs = case eldif of
    Left err -> Left $ transposePos ptab err -- get original line number
    Right ldif -> Right ldif
    where
        (input, ptab) = preproc xs
        eldif = parse pLdif name input

-- | Parsec ldif parser
pLdif :: Parser [(DN, LDIFRecord)]
pLdif = do
    pSEPs
    void $ optionMaybe pVersionSpec
    recs <- sepEndBy pRec pSEPs1
    void $ optionMaybe pSearchResult
    eof
    return recs
    where
        pVersionSpec = do
            void $ string "version:"
            pFILL
            xs <- many1 digit
            pSEPs1
            return xs
        pSearchResult = do
            void $ string "search:"
            pFILL
            void $ many1 digit
            pSEP
            void $ string "result:"
            pFILL
            void pSafeString
            pSEPs

pRec :: Parser (DN, LDIFRecord)
pRec = do
    dn <- pDNSpec
    pSEP
    try (pChangeRec dn) <|> pAttrValRec dn
    where
        pDNSpec = do
            void $ string "dn:"
            pDN
        pChangeRec dn = do
            void $ string "changetype:"
            pFILL
            r <- try pChangeAdd
                <|> try pChangeDel
                <|> try pChangeMod
            return (dn, r { rDn = dn })
        pAttrValRec dn = do
            r <- pLdapEntry
            return (dn, r { rDn = dn })


pLdapEntry :: Parser LDIFRecord
pLdapEntry = do
    attrVals <- sepEndBy1 pAttrValSpec pSEP
    return $ LDIFEntry [] (avToAttrs attrVals)

pChangeAdd :: Parser LDIFRecord
pChangeAdd = do
    void $ string "add"
    pSEP
    pLdapEntry

pChangeDel :: Parser LDIFRecord
pChangeDel = do
    void $ string "delete"
    pSEP
    void $ sepEndBy pAttrValSpec pSEP
    return $ LDIFDelete []

pChangeMod :: Parser LDIFRecord
pChangeMod = do
    void $ string "modify"
    pSEP
    mods <- sepEndBy1 pModSpec (char '-' >> pSEP)
    return . LDIFChange [] $ M.unions mods

pModSpec :: Parser (Attrs (LDAPModOp, Value))
pModSpec = do
   modStr <- pModType
   pFILL
   void pSafeString
   pSEP
   attrs <- sepEndBy pAttrValSpec pSEP
   return $ mkMod modStr attrs

mkMod :: String -> [AttrSpec] -> Attrs (LDAPModOp, Value)
mkMod modStr av
    | modStr == "add:" = toRec LdapModAdd
    | modStr == "delete:" = toRec LdapModDelete
    | modStr == "replace:" = toRec LdapModReplace
    | otherwise = error $ "unexpected mod:" ++ modStr
    where
        toRec op = avToAttrs $ map (second (zip (repeat op))) av

avToAttrs :: (Eq a, Hashable a) => [(Attribute, [a])] -> Attrs a
avToAttrs av =
    foldl' (\acc (a, s) -> M.insertWith S.union a s acc) M.empty avSets
    where
        avSets = map (second S.fromList) av

pDN :: Parser DN
pDN = do
    pFILL
    pSafeString'

pModType :: Parser String
pModType = try (string "add:")
       <|> try (string "delete:")
       <|> string "replace:"

pAttributeType :: Parser Attribute
pAttributeType = do
    pFILL
    c <- noneOf "-\n"
    xs <- many (noneOf " :\n")
    _ <- char ':'
    let ys = c:xs
    return ys

pAttrValSpec :: Parser (Attribute, [Value])
pAttrValSpec = do
    name <- pAttributeType
    pFILL
    val  <- pSafeString'
    name `seq` val `seq` return (name, [val])

pSafeString :: Parser String
pSafeString = do
    c <- noneOf "\n :<"
    r <- many (noneOf "\n")
    let xs = c:r
    return xs

pSafeString' :: Parser String
pSafeString' = many (noneOf "\n")

pFILL :: Parser ()
pFILL = skipMany (oneOf " \t")

pSEP :: Parser ()
pSEP = void newline

pSEPs :: Parser ()
pSEPs = void (many pSEP)

pSEPs1 :: Parser ()
pSEPs1 = void (many1 pSEP)

