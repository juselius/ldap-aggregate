{-| The module emits data structures directly usable by the LDAP package.

    This is a simplified version of Text.LDIF.Parser. It expexts valid and
    well formed LDIF as input. It does no actual parsing and validation of
    entries.

    <jonas.juselius@uit.no> 2014
-}
{-# LANGUAGE OverloadedStrings #-}

module LDIF.Simple (
      parseLDIFStr
    , LDIF(..)
    , LDIFEntry(..)
    , Attribute
    , Value
    , AttrSpec
) where

import Prelude
import Data.List
import Control.Monad
import Text.LDIF.Preproc
import LDAP.Modify (LDAPModOp(..))
import Text.Parsec as PR
import Text.Parsec.ByteString
import qualified Data.ByteString.Char8 as BC

--import Debug.Trace
type DN = String
type Attribute = String
type Value = String

type AttrSpec = (Attribute, [Value])

data LDIFEntry = LDIFEntry {
          ldifOp    :: LDAPModOp
        , ldifDN    :: DN
        , ldifAttrs :: [AttrSpec]
    } deriving (Eq)

data LDIF =
      LDIF { ldifEntry :: LDIFEntry }
    | LDIFMod { ldifEntry :: LDIFEntry } deriving (Eq)

instance Show LDIF where
    show (LDIF x) = "LDIFEntry -> " ++ show x
    show (LDIFMod x) = "LDIFMod -> " ++ show x

instance Show LDIFEntry where
    show (LDIFEntry op dn attrs) = "dn: " ++ dn ++ "\n"
        ++ (init . foldl (\s a ->
            s ++ "    " ++ show a ++ "\n") "" $ attrs)
        ++ "\n    <" ++ show op ++ ">\n"

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
            attrVals `seq` return [LDIF $ LDIFEntry LdapModAdd dn attrVals]
        pChangeRec :: DN -> Parser [LDIF]
        pChangeRec dn = do
            void $ string "changetype:"
            pFILL
            try (pChangeAdd dn)
                <|> try (pChangeDel dn)
                <|> try (pChangeMod dn)
                <|> pChangeModDN dn
        collectAttrs :: [LDIF] -> [LDIF]
        collectAttrs = map collect
            where
                collect (LDIF l@(LDIFEntry _ _ attrs))
                    = LDIF l {ldifAttrs = reGroup attrs}
                collect (LDIFMod l@(LDIFEntry _ _ attrs))
                    = LDIFMod l {ldifAttrs = reGroup attrs}
                reGroup xs = map gather $ groupBy (\(a, _) (b, _) -> a == b) xs
                gather xs = (fst (head xs), concatMap snd xs)

pChangeAdd :: DN -> Parser [LDIF]
pChangeAdd dn = do
    void $ string "add"
    pSEP
    attrs <- sepEndBy1 pAttrValSpec pSEP
    return [LDIF $ LDIFEntry LdapModAdd dn attrs]

pChangeDel :: DN -> Parser [LDIF]
pChangeDel dn = do
    void $ string "delete"
    pSEP
    attrs <- sepEndBy pAttrValSpec pSEP
    return [LDIF $ LDIFEntry LdapModAdd dn attrs]

pChangeMod :: DN -> Parser [LDIF]
pChangeMod dn = do
    void $ string "modify"
    pSEP
    sepEndBy1 (pModSpec dn) (char '-' >> pSEP)

pChangeModDN :: DN -> Parser [LDIF]
pChangeModDN dn = do
    void $ string "modrdn"
    pSEP
    void $ string "newrdn:"
    pFILL
    void pRDN
    pSEP
    void $ string "deleteoldrdn:"
    pFILL
    void $ oneOf "01"
    pSEP
    return [LDIF $ LDIFEntry  LdapModDelete dn []]

pRDN :: Parser String
pRDN = pSafeString

pDN :: Parser DN
pDN = do
    pFILL
    pSafeString'

pModSpec :: String -> Parser LDIF
pModSpec dn = do
   modStr <- pModType
   pFILL
   void pSafeString  -- attribute type to modify (ignore)
   pSEP
   attrs <- sepEndBy pAttrValSpec pSEP
   return $ mkMod modStr dn attrs

mkMod :: String -> String -> [AttrSpec] -> LDIF
mkMod modStr dn attrs
    | modStr == "add:" = LDIFMod $ LDIFEntry LdapModAdd dn attrs
    | modStr == "delete:" = LDIFMod $ LDIFEntry LdapModDelete dn attrs
    | modStr == "replace:" = LDIFMod $ LDIFEntry LdapModReplace dn attrs
    | otherwise = error $ "unexpected mod:" ++ modStr
    -- error can not be reached because pModType

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

