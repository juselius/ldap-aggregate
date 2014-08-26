{-| The module emits data structures directly usable by the LDAP package.

    This is a simplified version of Text.LDIF.Parser. It expexts valid and
    well formed LDIF as input. It does no actual parsing and validation of
    entries.

    <jonas.juselius@uit.no> 2014
-}
{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module LDIF.Simple (
      parseLDIFStr
)
where
import Prelude
import Text.LDIF.Preproc
import LDAP.Modify (LDAPMod(..), LDAPModOp(..))
import LDAP.Search (LDAPEntry(..))
import Text.Parsec as PR
import Text.Parsec.ByteString
import qualified Data.ByteString.Char8 as BC

data LDIF = LDIFRecord {
          entry :: LDAPEntry
    }
    | LDIFMod {
          modDN :: String
        , mod :: LDAPMod
    } deriving (Show)

type DN = String

type Attribute = String

type Value = String

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
    optionMaybe pVersionSpec
    recs <- sepEndBy pRec pSEPs1
    optionMaybe pSearchResult
    eof
    return $ concat recs
    where
        pVersionSpec :: Parser String
        pVersionSpec = do
            string "version:"
            pFILL
            xs <- many1 digit
            pSEPs1
            let ys = xs `seq` xs
            ys `seq` return $ ys
        pSearchResult :: Parser ()
        pSearchResult = do
            string "search:"
            pFILL
            many1 digit
            pSEP
            string "result:"
            pFILL
            pSafeString
            pSEPs
            return ()

pRec :: Parser [LDIF]
pRec = do
    dn <- pDNSpec
    pSEP
    try (pChangeRec dn) <|> (pAttrValRec dn)
    where
        pDNSpec :: Parser DN
        pDNSpec = do
            string "dn:"
            pDN
        pAttrValRec :: DN -> Parser [LDIF]
        pAttrValRec dn = do
            attrVals <- sepEndBy1 pAttrValSpec pSEP
            attrVals `seq` return $ [LDIFRecord (LDAPEntry dn attrVals)]
        pChangeRec :: DN -> Parser [LDIF]
        pChangeRec dn = do
            string "changetype:"
            pFILL
            try (pChangeAdd dn)
                <|> try (pChangeDel dn)
                <|> try (pChangeMod dn)
                <|> (pChangeModDN dn)

pChangeAdd :: DN -> Parser [LDIF]
pChangeAdd dn = do
    string "add"
    pSEP
    vals <- sepEndBy1 pAttrValSpec pSEP
    return $ map (\(a, v) -> LDIFMod dn $ LDAPMod LdapModAdd a v) vals

pChangeDel :: DN -> Parser [LDIF]
pChangeDel dn = do
    string "delete"
    pSEP
    vals <- sepEndBy pAttrValSpec pSEP
    return $ case vals of
        [] -> [LDIFMod dn $ LDAPMod LdapModDelete "" []] -- delete entire record
        _ -> map (\(a, v) -> LDIFMod dn $ LDAPMod LdapModDelete a v) vals

pChangeMod :: DN -> Parser [LDIF]
pChangeMod dn = do
    string "modify"
    pSEP
    mods <- sepEndBy1 pModSpec (char '-' >> pSEP)
    return $ map (LDIFMod dn) mods

pChangeModDN :: DN -> Parser [LDIF]
pChangeModDN dn = do
    string "modrdn"
    pSEP
    string "newrdn:"
    pFILL
    pRDN
    pSEP
    string "deleteoldrdn:"
    pFILL
    oneOf "01"
    pSEP
    return [LDIFMod dn $ LDAPMod LdapModDelete dn []]

pRDN :: Parser String
pRDN = pSafeString

pDN :: Parser DN
pDN = do
    pFILL
    dn <- pSafeString'
    dn `seq` return dn

pModSpec :: Parser LDAPMod
pModSpec = do
   modType' <- pModType
   pFILL
   att <- pSafeString
   pSEP
   avals <- sepEndBy pAttrValSpec pSEP
   let vals = concat $ map snd avals
   return $ mkMod modType' att vals

mkMod :: String -> Attribute -> [Value] -> LDAPMod
mkMod modType' att vals
    | modType' == "add:" = LDAPMod LdapModAdd att vals
    | modType' == "delete:" = LDAPMod LdapModDelete att vals
    | modType' == "replace:" = LDAPMod LdapModReplace att vals
    | otherwise = error $ "unexpected mod:" ++ modType'
    -- error can not be reached because pModType

pModType :: Parser String
pModType = try (string "add:")
       <|> try (string "delete:")
       <|> string "replace:"

pAttributeType :: Parser Attribute
pAttributeType = do
    pFILL
    c <- noneOf "-"
    xs <- many (noneOf " :\n")
    char ':'
    let ys = c:xs
    ys `seq` return ys

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
    let xs = r `seq` c:r
    let ys = xs `seq` xs
    ys `seq` return ys

pSafeString' :: Parser String
pSafeString' = do
    r <- many (noneOf "\n")
    let ys = r `seq` r
    ys `seq` return ys

pFILL :: Parser ()
pFILL = skipMany (oneOf [' ', '\t'])

pSEP :: Parser ()
pSEP = newline >> return ()

pSEPs :: Parser ()
pSEPs = many pSEP >> return ()

pSEPs1 :: Parser ()
pSEPs1 = many1 pSEP >> return ()


