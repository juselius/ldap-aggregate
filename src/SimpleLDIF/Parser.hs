{-| The module emits data structures directly usable by the LDAP package.

    This is a simplified version of Text.LDIF.Parser, originally by
    Radoslav Dorick <radoslav.dorick@gmail.com>. It expexts valid and
    well formed LDIF as input. It does no actual parsing and validation of
    entries.

    <jonas.juselius@uit.no> 2014
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module SimpleLDIF.Parser (
      parseLdifStr
    , parseLdifStr'
    , parseLdif
    , parseLdif'
) where

import Prelude
import Data.List
import Control.Monad
import SimpleLDIF.Types
import SimpleLDIF.Preproc
import Data.Hashable
import "parsec" Text.Parsec as PR
import "parsec" Text.Parsec.Text
import Control.Arrow (second)
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S

parseLdif :: T.Text -> LDIF
parseLdif ldif = either (error . show) id (parseLdifStr [] ldif)

parseLdif' :: T.Text -> [Ldif]
parseLdif' ldif = either (error . show) id (parseLdifStr' [] ldif)

-- | Parse LDIF content
parseLdifStr :: FilePath -> T.Text -> Either ParseError LDIF
parseLdifStr name xs = case parseLdifStr' name xs of
    Left err -> Left err
    Right ldif -> Right $ M.fromList ldif

parseLdifStr' :: FilePath -> T.Text -> Either ParseError [Ldif]
parseLdifStr' name xs = case eldif of
    Left err -> Left $ transposePos ptab err -- get original line number
    Right ldif -> Right ldif
    where
        (input, ptab) = preproc xs
        eldif = parse pLdif name input

-- | Parsec ldif parser
pLdif :: Parser [Ldif]
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
            return (dn, r { rDn = dn })
        pAttrValRec dn = do
            r <- pLdapEntry
            return (dn, r { rDn = dn })


pLdapEntry :: Parser LDIFRecord
pLdapEntry = do
    attrVals <- sepEndBy1 pAttrValSpec pSEP
    return $ LDIFAdd T.empty (avToAttrs attrVals)

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
    return $ LDIFDelete T.empty

pChangeMod :: Parser LDIFRecord
pChangeMod = do
    void $ string "modify"
    pSEP
    mods <- sepEndBy1 pModSpec (char '-' >> pSEP)
    return . LDIFChange T.empty $ M.unions mods

pModSpec :: Parser (LdifAttrs (LDAPModOp, LdifValue))
pModSpec = do
   modStr <- pModType
   pFILL
   void pSafeString
   pSEP
   attrs <- sepEndBy pAttrValSpec pSEP
   return $ mkMod modStr attrs

mkMod :: T.Text
      -> [(LdifAttr, [LdifValue])]
      -> LdifAttrs (LDAPModOp, LdifValue)
mkMod modStr av
    | modStr == "add:" = toRec LdapModAdd
    | modStr == "delete:" = toRec LdapModDelete
    | modStr == "replace:" = toRec LdapModReplace
    | otherwise = error $ "unexpected mod:" ++ T.unpack modStr
    where
        toRec op = avToAttrs $ map (second (zip (repeat op))) av

avToAttrs :: (Eq a, Hashable a) => [(LdifAttr, [a])] -> LdifAttrs a
avToAttrs av =
    foldl' (\acc (a, s) -> M.insertWith S.union a s acc) M.empty avSets
    where
        avSets = map (second S.fromList) av

pDN :: Parser DN
pDN = do
    pFILL
    pSafeString'

pModType :: Parser T.Text
pModType = fmap T.pack $
           try (string "add:")
       <|> try (string "delete:")
       <|> string "replace:"

pAttrType :: Parser LdifAttr
pAttrType = do
    pFILL
    c <- noneOf "-\n"
    xs <- many (noneOf " :\n")
    _ <- char ':'
    let ys = c:xs
    return $ T.pack ys

pAttrValSpec :: Parser (LdifAttr, [LdifValue])
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
    return $ T.pack xs

pSafeString' :: Parser T.Text
pSafeString' = fmap T.pack $ many (noneOf "\n")

pFILL :: Parser ()
pFILL = skipMany (oneOf " \t")

pSEP :: Parser ()
pSEP = void newline

pSEPs :: Parser ()
pSEPs = void (many pSEP)

pSEPs1 :: Parser ()
pSEPs1 = void (many1 pSEP)

