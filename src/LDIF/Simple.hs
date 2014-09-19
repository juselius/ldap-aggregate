{-| The module emits data structures directly usable by the LDAP package.

    This is a simplified version of Text.LDIF.Parser. It expexts valid and
    well formed LDIF as input. It does no actual parsing and validation of
    entries.

    <jonas.juselius@uit.no> 2014
-}
{-# LANGUAGE OverloadedStrings, PackageImports #-}

module LDIF.Simple (
      parseLDIFStr
    , LDIF(..)
    , LDIFRecord(..)
    , Attribute
    , Value
    , AttrSpec
    , DN
) where

import Prelude
import Data.List
import Control.Monad
import LDIF.Types
import LDIF.Preproc
import "parsec" Text.Parsec as PR
import "parsec" Text.Parsec.ByteString
import qualified Data.ByteString.Char8 as BC

--import Debug.Trace

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
    return $ concat recs
    where
        pVersionSpec :: Parser String
        pVersionSpec = do
            void $ string "version:"
            pFILL
            xs <- many1 digit
            pSEPs1
            return xs
        pSearchResult :: Parser ()
        pSearchResult = do
            void $ string "search:"
            pFILL
            void $ many1 digit
            pSEP
            void $ string "result:"
            pFILL
            void pSafeString
            pSEPs

pRec :: Parser [LDIF]
pRec = do
    dn <- pDNSpec
    pSEP
    attrs <- try (pChangeRec dn) <|> pAttrValRec dn
    return $ collectAttrs attrs
    where
        pDNSpec :: Parser DN
        pDNSpec = do
            void $ string "dn:"
            pDN
        pAttrValRec :: DN -> Parser [LDIF]
        pAttrValRec dn = do
            attrVals <- sepEndBy1 pAttrValSpec pSEP
            attrVals `seq` return [LDIF (dn, LDIFEntry $ LDIFContents attrVals)]
        pChangeRec :: DN -> Parser [LDIF]
        pChangeRec dn = do
            void $ string "changetype:"
            pFILL
            r <- try pChangeAdd
                <|> try pChangeDel
                <|> try pChangeMod
            return $ map (\x -> LDIF (dn, x)) r
        collectAttrs :: [LDIF] -> [LDIF]
        collectAttrs = map collect
            where
                collect (LDIF x@(dn, r@rec)) =
                    case rec of
                        LDIFDelete -> LDIF x
                        c@(LDIFChange _ attrs) ->
                            LDIF (dn, c { ldifMods = reGroup attrs })
                        _ -> LDIF (dn, r { ldifContents =
                                reGroupContents (ldifContents r) })
                reGroup xs =
                    map gather . groupBy (\(a, _) (b, _) -> a == b) $ xs
                reGroupContents (LDIFContents c) = LDIFContents $ reGroup c
                gather xs = (fst (head xs), concatMap snd xs)

pChangeAdd :: Parser [LDIFRecord]
pChangeAdd = do
    void $ string "add"
    pSEP
    attrs <- sepEndBy1 pAttrValSpec pSEP
    return [LDIFAdd $ LDIFContents attrs]

pChangeDel :: Parser [LDIFRecord]
pChangeDel = do
    void $ string "delete"
    pSEP
    void $ sepEndBy pAttrValSpec pSEP
    return [LDIFDelete]

pChangeMod :: Parser [LDIFRecord]
pChangeMod = do
    void $ string "modify"
    pSEP
    sepEndBy1 pModSpec (char '-' >> pSEP)

pModSpec :: Parser LDIFRecord
pModSpec = do
   modStr <- pModType
   pFILL
   void pSafeString  -- attribute type to modify (ignore)
   pSEP
   attrs <- sepEndBy pAttrValSpec pSEP
   return $ mkMod modStr attrs

mkMod :: String -> [AttrSpec] -> LDIFRecord
mkMod modStr attrs
    | modStr == "add:" = LDIFChange LdapModAdd attrs
    | modStr == "delete:" = LDIFChange LdapModDelete attrs
    | modStr == "replace:" = LDIFChange LdapModReplace attrs
    | otherwise = error $ "unexpected mod:" ++ modStr
    -- error can not be reached because pModType

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

