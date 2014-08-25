{-| The module emits data structures directly usable by the LDAP package.

    This is a simplified version of Text.LDIF.Parser.

    <jonas.juselius@uit.no> 2014
-}
{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module LDIF.Parser (
	  parseLDIFStr
    , preproc
)
where
import Prelude
import LDIF.Preproc
import LDAP.Modify (LDAPMod(..), LDAPModOp(..))
import LDAP.Search (LDAPEntry(..))
import Text.Parsec as PR
import Text.Parsec.ByteString
import Text.Parsec.Pos (initialPos)
import Text.Parsec.Error (Message(..), newErrorMessage)
import qualified Data.ByteString.Char8 as BC
import Data.Char
import Data.Maybe (fromJust, isNothing)
import Numeric (readHex)

data LDIF =
      LDIFRecord { entry :: LDAPEntry }
    | LDIFMod    { modDN :: String, mod :: LDAPMod }

type DN = String

type Attribute = String

type Value = String

-- | Parse LDIF content
parseLDIFStr :: FilePath -> BC.ByteString -> Either ParseError [LDIF]
parseLDIFStr name xs = parse pLdif name (preproc xs)

-- | Parsec ldif parser
pLdif :: Parser [LDIF]
pLdif = do
    nls
    optionMaybe pVersionSpec
    recs <- sepEndBy pRec nls1
    optionMaybe pSearchResult
    eof
    return $ concat recs
    where
        pVersionSpec :: Parser String
        pVersionSpec = do
            string "version:"
            whitespace
            xs <- many1 digit
            nls1
            let ys = xs `seq` xs
            ys `seq` return $ ys
        pSearchResult :: Parser ()
        pSearchResult = do
            string "search:"
            whitespace
            many1 digit
            nl
            string "result:"
            whitespace
            pSafeString
            nls
            return ()

pRec :: Parser [LDIF]
pRec = do
    dn <- pDNSpec
    nl
    try (pChangeRec dn) <|> (pAttrValRec dn)
    where
        pDNSpec :: Parser String
        pDNSpec = do
            string "dn:"
            pDN
        pAttrValRec :: DN -> Parser [LDIF]
        pAttrValRec dn = do
            attrVals <- sepEndBy1 pAttrValSpec nl
            attrVals `seq` return $ [LDIFRecord (LDAPEntry dn attrVals)]
        pChangeRec :: DN -> Parser [LDIF]
        pChangeRec dn = do
            string "changetype:"
            whitespace
            try (pChangeAdd dn)
                <|> try (pChangeDel dn)
                <|> try (pChangeMod dn)
                <|> (pChangeModDN dn)

pChangeAdd :: DN -> Parser [LDIF]
pChangeAdd dn = do
    string "add"
    nl
    vals <- sepEndBy1 pAttrValSpec nl
    return $ map (\(a, v) -> LDIFMod dn $ LDAPMod LdapModAdd a v) vals

pChangeDel :: DN -> Parser [LDIF]
pChangeDel dn = do
    string "delete"
    nl
    vals <- sepEndBy pAttrValSpec nl
    return $ [LDIFMod dn $ LDAPMod LdapModDelete dn []]

pChangeMod :: DN -> Parser [LDIF]
pChangeMod dn = do
    string "modify"
    nl
    mods <- sepEndBy1 pModSpec (char '-' >> nl)
    return $ map (LDIFMod dn) mods

pChangeModDN :: DN -> Parser [LDIF]
pChangeModDN dn = do
    string "modrdn"
    nl
    string "newrdn:"
    whitespace
    pRDN
    nl
    string "deleteoldrdn:"
    whitespace
    oneOf "01"
    nl
    return [LDIFMod dn $ LDAPMod LdapModDelete dn []]

pRDN :: Parser String
pRDN = pSafeString

pDN :: Parser DN
pDN = do
    whitespace
    dn <- sepEndBy pAttrEqValue (char ',')
    dn `seq` return dn

pAttrEqValue :: Parser String
pAttrEqValue = do
    whitespace
    att <- pAttributeType
    char '='
    val <- pAttrValueDN
    return $ att ++ val

pAttrValueDN :: Parser Value
pAttrValueDN = do
    xs <- many1 allChar
    let ys = xs `seq` xs
    ys `seq` return $ ys
    where
        allChar = noneOf (escapedDNChars ++ "\n")
            <|> try (hexChar)
            <|> (escChar)
        escChar = do
            char '\\'
            oneOf escapedDNChars
        hexChar = do
            char '\\'
            hval <- PR.count 2 hexDigit
            case readHex hval of
                [(val,[])] -> return $ chr val
                _ -> fail $ "invalid hex value: " ++ hval

pModSpec :: Parser LDAPMod
pModSpec = do
   modType <- pModType
   whitespace
   att <- pAttributeType
   nl
   vals <- sepEndBy pAttrValSpec nl
   return $ mkMod modType att vals

mkMod :: String -> Attribute -> [Value] -> LDAPMod
mkMod modType att vals
    | modType == "add:" = LDAPMod LdapModAdd att vals
    | modType == "delete:" = LDAPMod LdapModDelete att vals
    | modType == "replace:" = LDAPMod LdapModReplace att vals
    | otherwise = error $ "unexpected mod:" ++ modType
    -- error can not be reached because pModType

pModType :: Parser String
pModType = try (string "add:")
       <|> try (string "delete:")
       <|> string "replace:"

pAttributeType :: Parser Attribute
pAttributeType = try pLdapOid <|> pCharType
    where
        pDotOid :: Parser String
        pDotOid = do
            string "."
            n <- many1 digit
            let xs = n `seq` '.':n
            xs `seq` return xs
        pLdapOid :: Parser String
        pLdapOid = do
            num <- many1 digit
            rest <- many1 pDotOid
            let xs = num `seq` rest `seq` num ++ concat rest
            xs `seq` return xs
        pCharType :: Parser String
        pCharType = do
            l <- letter
            o <- pAttrTypeChars
            let xs = l `seq` o `seq` l:o
            xs `seq` return  xs
        pAttrTypeChars :: Parser String
        pAttrTypeChars = do
            xs <- many (satisfy (\x -> isAlphaNum x || x == '-'))
            let ys = xs `seq` BC.pack xs
            ys `seq` return ys


pAttrValSpec :: Parser (Attribute, [Value])
pAttrValSpec = do
    name <- pAttributeType
    val  <- pValueSpec
    name `seq` val `seq` return (name, [val])
    where
        pValueSpec :: Parser Value
        pValueSpec = try (char ':' >> whitespace >> pSafeString')
            <|> try (char ':' >> char ':' >> whitespace >> pBase64String)
            <|> (char ':' >> char '<' >> whitespace >> pURL)

pURL :: Parser String
pURL = pSafeString

pSafeString :: Parser String
pSafeString = do
    c <- noneOf "\n :<"
    r <- many (noneOf "\n")
    let xs = r `seq` c:r
    let ys = xs `seq` xs
    ys `seq` return ys

pSafeString' :: Parser String
pSafeString' = do
    r <- many (noneOf "\n")
    let ys = r `seq` r
    ys `seq` return ys

pBase64String :: Parser String
pBase64String = pSafeString

whitespace :: Parser ()
whitespace = skipMany (oneOf [' ', '\t'])

nl :: Parser ()
nl = newline >> return ()

nls :: Parser ()
nls = many nl >> return ()

nls1 :: Parser ()
nls1 = many1 nl >> return ()

-- | Chars with special meaning in DN
specialDNChars :: [Char]
specialDNChars = [',','=','+','<','>','#',';','/']

-- | Chars necessary to be escaped in DN when they are part of value
escapedDNChars :: [Char]
escapedDNChars = ['\\', '"'] ++ specialDNChars


