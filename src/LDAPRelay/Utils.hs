--
-- <jonas.juselius@uit.no> 2014
--

module LDAPRelay.Utils (
      bindDIT
    , printDIT
    , getDIT
    , askBindPw
    , ldata2LdapMod
    , modifyTreeFromLdif
    , rewriteDN
    , rewriteAttrs
    --, LDAPEntry(..)
    --, LDAPMod(..)
) where

import System.IO
import LDAP
import LDIF.Simple
import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Text.RegexPR as PR

type RegexFrom = String
type RegexTo = String
type AttrName = String
type FromTo = (RegexFrom, RegexTo)
type AttrFromTo = (AttrName, (RegexFrom, RegexTo))
type AttrList = [AttrName]

printDIT :: LDAP -> String -> IO ()
printDIT ldap tree = do
    ldif <- getDIT ldap tree
    putStrLn . init . foldl prettify  "" $ ldif
    putStrLn "--"
    where
        prettify s e@(LDAPEntry _ _) = show (LDAPEntry' e) ++ "\n" ++ s

bindDIT :: String -> IO LDAP
bindDIT tree = do
    --pw <- askBindPw
    ldap <- ldapInitialize "ldap://localhost:389"
    ldapSimpleBind ldap ("cn=admin," ++ tree) "secret"
    return ldap

getDIT :: LDAP -> String -> IO [LDAPEntry]
getDIT ldap tree =
    ldapSearch ldap (Just tree) LdapScopeSubtree Nothing LDAPAllUserAttrs False

askBindPw :: IO String
askBindPw = do
    putStr "bindpw: "
    hFlush stdout
    hSetEcho stdout False
    pw <- getLine
    hSetEcho stdout True
    putStrLn ""
    return pw

modifyTreeFromLdif :: LDAP -> BS.ByteString -> IO ()
modifyTreeFromLdif ldap ldif =
    mapM_ (\(LDIFMod dn attrs) ->
            ldapModify ldap dn [attrs]) $
                either (error . show) id (parseLDIFStr "" ldif)

ldata2LdapMod :: BS.ByteString -> [(String, [LDAPMod])]
ldata2LdapMod str =
        map (\(LDAPEntry dn attrs) ->
            (dn, list2ldm LdapModAdd attrs)) ldif
    where
        ldif = extractEntries $ parseLDIFStr "" str
        extractEntries =
            either (error . show) (map ldapEntry)

rewriteDN :: [FromTo] -> LDIF -> Maybe LDIF
rewriteDN fts x@(LDIFEntry _ y@(LDAPEntry dn _)) =
    case substDN fts dn of
        Just dn' -> Just $ x { ldapEntry = y {ledn = dn'} }
        Nothing -> Nothing
rewriteDN fts x@(LDIFMod dn _) =
    case substDN fts dn of
        Just dn' -> Just $ x { modDN = dn' }
        Nothing -> Nothing

substDN :: [FromTo] -> String -> Maybe String
substDN fts dn = case getFirstMatch of
        [str] -> Just str
        _ -> Nothing
    where
        getFirstMatch = take 1 . dropWhile (== dn) $
            map (\(src, dst) -> PR.subRegexPR src dst dn) fts

rewriteAttrs :: [AttrFromTo] -> LDIF -> Maybe LDIF
rewriteAttrs afts  x@(LDIFEntry _ y@(LDAPEntry _ attrs)) =
    case result of
        Just attrs' ->
            Just $ x {ldapEntry = y {leattrs = attrs'}}
        Nothing -> Nothing
        where
            (update, retained) = partition (\p ->
                isJust $ lookup (fst p) attrs) afts
            updated = []
            result = updated ++ retained
rewriteAttrs afts x@(LDIFMod _ y@(LDAPMod _ attr vals)) =
    case Nothing of
        Just (a, v) ->
            Just $ x {modEntry = y {modType = a, modVals = v }}
        Nothing -> Nothing

filterAttrs :: AttrList -> LDIF -> LDIF
filterAttrs attrl ldif = ldif


