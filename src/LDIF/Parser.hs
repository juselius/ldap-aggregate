{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase #-}
------------------------------------------------------------------------
-- |
-- Module    : LDIF.Parser
-- Copyright : Jonas Juselius 2014
--
-- The module emits data structures easily usable by the LDAP package.
--
-- This is a simplified version of Text.LDIF.Parser, originally by
-- Radoslav Dorick <radoslav.dorick@gmail.com>. It expexts valid and
-- well formed LDIF as input. It does no actual parsing and validation of
-- entries.
------------------------------------------------------------------------
module LDIF.Parser (
      parseLdif
    , parseLdifEither
) where

import Prelude
import Data.List
import Control.Monad
import LDIF.Types
import LDIF.Preproc
import Data.Hashable
import "parsec" Text.Parsec as PR
import "parsec" Text.Parsec.Text
import Control.Arrow (second)
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

data Ldif = LdifRec (DN, LDIFRecord) | LdifMod (DN, LDIFMod)

parseLdif :: T.Text -> LDIF
parseLdif ldif = either (error . show) id (parseLdifEither ldif)

-- | Parse LDIF content
parseLdifEither :: T.Text -> Either ParseError LDIF
parseLdifEither xs = case eldif of
    Left err -> Left $ transposePos ptab err -- get original line number
    Right ldif -> Right ldif
    where
        (input, ptab) = preproc xs
        eldif = parse pLdif [] input

-- | Parsec ldif parser
pLdif :: Parser LDIF
pLdif = do
    pSEPs
    void $ optionMaybe pVersionSpec
    recs <- sepEndBy pRec pSEPs1
    void $ optionMaybe pSearchResult
    eof
    return $ foldl' l2L lempty recs
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
        lempty = LDIF HM.empty HM.empty
        l2L (LDIF r o) = \case
            LdifRec (dn, x) -> LDIF (HM.insert dn x r) o
            LdifMod  (dn, x) -> LDIF r (HM.insert dn x o)

pRec :: Parser Ldif
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
            return . LdifMod $ (dn, r { modDn = dn })
        pAttrValRec dn = do
            r <- pLdapEntry
            return . LdifRec $ (dn, r { rDn = dn })


pLdapEntry :: Parser LDIFRecord
pLdapEntry = do
    attrVals <- sepEndBy1 pAttrValSpec pSEP
    return $ LDIFRecord T.empty (avToAttrs attrVals)

pChangeAdd :: Parser LDIFMod
pChangeAdd = do
    void $ string "add"
    pSEP
    LDIFRecord dn a <- pLdapEntry
    return  $ LDIFAdd dn a

pChangeDel :: Parser LDIFMod
pChangeDel = do
    void $ string "delete"
    pSEP
    void $ sepEndBy pAttrValSpec pSEP
    return $ LDIFDelete T.empty

pChangeMod :: Parser LDIFMod
pChangeMod = do
    void $ string "modify"
    pSEP
    mods <- sepEndBy1 pModSpec (char '-' >> pSEP)
    return $ LDIFChange T.empty $ HM.unions mods

pModSpec :: Parser (LDIFAttrs (LDAPModOp, Value))
pModSpec = do
   modStr <- pModType
   pFILL
   void pSafeString
   pSEP
   attrs <- sepEndBy pAttrValSpec pSEP
   return $ mkMod modStr attrs

mkMod :: T.Text
      -> [(Attr, [Value])]
      -> LDIFAttrs (LDAPModOp, Value)
mkMod modStr av
    | modStr == "add:" = toRec LdapModAdd
    | modStr == "delete:" = toRec LdapModDelete
    | modStr == "replace:" = toRec LdapModReplace
    | otherwise = error $ "unexpected mod:" ++ T.unpack modStr
    where
        toRec op = avToAttrs $ map (second (zip (repeat op))) av

avToAttrs :: (Eq a, Hashable a) => [(Attr, [a])] -> LDIFAttrs a
avToAttrs av =
    foldl' (\acc (a, s) -> HM.insertWith HS.union a s acc) HM.empty avSets
    where
        avSets = map (second HS.fromList) av

pDN :: Parser DN
pDN = do
    pFILL
    pSafeString'

pModType :: Parser T.Text
pModType = fmap T.pack $
           try (string "add:")
       <|> try (string "delete:")
       <|> string "replace:"

pAttrType :: Parser Attr
pAttrType = do
    pFILL
    c <- noneOf "-\n"
    xs <- many (noneOf " :\n")
    _ <- char ':'
    let ys = c:xs
        t  = T.pack ys
    t `seq` return t

pAttrValSpec :: Parser (Attr, [Value])
pAttrValSpec = do
    name <- pAttrType
    pFILL
    val  <- pSafeString'
    name `seq` val `seq` return (name, [val])

pSafeString :: Parser T.Text
pSafeString = do
    c <- noneOf "\n :<"
    r <- many (noneOf "\n")
    let xs = c:r
        t  = T.pack xs
    t `seq` return t

pSafeString' :: Parser T.Text
pSafeString' = T.pack <$> many (noneOf "\n")

pFILL :: Parser ()
pFILL = skipMany (oneOf " \t")

pSEP :: Parser ()
pSEP = void newline

pSEPs :: Parser ()
pSEPs = void (many pSEP)

pSEPs1 :: Parser ()
pSEPs1 = void (many1 pSEP)

