{-| The module emits data structures directly usable by the LDAP package.

    This is a simplified version of Text.LDIF.Parser. It expexts valid and
    well formed LDIF as input. It does no actual parsing and validation of
    entries.

    <jonas.juselius@uit.no> 2014
-}
{-# LANGUAGE OverloadedStrings, PackageImports #-}

module LDIF.Parser (
      parseLDIFStr
    , LDIF(..)
    , LDIFRecord(..)
) where

import Prelude
import Data.List
import Control.Monad
import LDIF.Types
import LDIF.Preproc
import "parsec" Text.Parsec as PR
import "parsec" Text.Parsec.ByteString
import qualified Data.ByteString.Char8 as BC

-- | Parse LDIF content
parseLDIFStr :: FilePath -> BC.ByteString -> Either ParseError [LDIF]
parseLDIFStr name xs = case eldif of
    Left err -> Left $ transposePos ptab err -- get original line number
    Right ldif -> Right ldif
    where
        (input, ptab) = preproc xs
        eldif = parse pLdif name input

-- | Parsec ldif parser
pLdif :: Parser [LDIF]
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

pRec :: Parser LDIF
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
            return $ LDIF (dn, r)
        pAttrValRec dn = do
            x <- pLdapEntry dn
            return $ LDIF (dn, LDIFEntry x)


pLdapEntry :: DN -> Parser LDAPEntry
pLdapEntry dn = do
    attrVals <- sepEndBy1 pAttrValSpec pSEP
    return $ LDAPEntry dn (collect attrVals)
    where
        collect = map gather . groupBy cmpfst
        gather xs = (fst (head xs), concatMap snd xs)
        cmpfst (a, _) (b, _) = a == b

pChangeAdd :: Parser LDIFRecord
pChangeAdd = do
    void $ string "add"
    pSEP
    entry <- pLdapEntry []
    return . LDIFAdd $ snd (entry2add entry)
    where

pChangeDel :: Parser LDIFRecord
pChangeDel = do
    void $ string "delete"
    pSEP
    void $ sepEndBy pAttrValSpec pSEP
    return LDIFDelete

pChangeMod :: Parser LDIFRecord
pChangeMod = do
    void $ string "modify"
    pSEP
    mods <- sepEndBy1 pModSpec (char '-' >> pSEP)
    return $ LDIFChange mods


pModSpec :: Parser LDAPMod
pModSpec = do
   modStr <- pModType
   pFILL
   void pSafeString
   pSEP
   attrs <- sepEndBy pAttrValSpec pSEP
   return $ mkMod modStr attrs

mkMod :: String -> [AttrSpec] -> LDAPMod
mkMod modStr av
    | modStr == "add:" = rec LdapModAdd
    | modStr == "delete:" = rec LdapModDelete
    | modStr == "replace:" = rec LdapModReplace
    | otherwise = error $ "unexpected mod:" ++ modStr
    where
        rec op = LDAPMod op attrName attrs
        attrName = fst . head $ av
        attrs = concat . map snd $ av

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

