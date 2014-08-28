module TestUtils (
      bindDIT
    , printDIT
    , getDIT
    , getBindPw
    , clearTree
    , clearTree'
    , populateSource
    , populateTarget
    , ldata2LdapMod
    , modifyTreeFromLdif
) where

import System.IO
import Text.Regex.Posix
import Data.Either
import LDAP
import LDIF.Simple
import qualified Data.ByteString.Char8 as BS

printDIT :: LDAP -> String -> IO ()
printDIT ldap tree = do
    ldif <- getDIT ldap tree
    putStrLn . init . foldl prettify  "" $ ldif
    putStrLn "--"
    where
        prettify s e@(LDAPEntry _ _) = show (LDAPEntry' e) ++ "\n" ++ s

bindDIT :: String -> IO LDAP
bindDIT tree = do
    --pw <- getBindPw
    ldap <- ldapInitialize "ldap://localhost:389"
    ldapSimpleBind ldap ("cn=admin," ++ tree) "secret"
    return ldap

getDIT :: LDAP -> String -> IO [LDAPEntry]
getDIT ldap tree =
    ldapSearch ldap (Just tree) LdapScopeSubtree Nothing LDAPAllUserAttrs False

getBindPw :: IO String
getBindPw = do
    putStr "bindpw: "
    hFlush stdout
    pw <- getLine
    putStrLn ""
    return pw

clearTree' :: String -> IO ()
clearTree' tree = do
    ldap <- bindDIT tree
    printDIT ldap tree
    clearTree ldap tree
    printDIT ldap tree

clearTree :: LDAP -> String -> IO ()
clearTree ldap tree = do
    ldif <- getDIT ldap tree
    let ldif' = reverse $ filter delEntry ldif
    mapM_ (\dn-> ldapDelete ldap (ledn dn)) ldif'
    where
        delEntry e
            | ledn e =~ "^dc=(source|target)$" :: Bool = False
            | ledn e =~ "^cn=admin,dc=[^=]*$" :: Bool = False
            | otherwise = True

populateSource :: LDAP -> IO ()
populateSource ldap = do
    ltree <- withFile "./ldif/source.tree.ldif" ReadMode BS.hGetContents
    lpops <- withFile "./ldif/source.populate.ldif" ReadMode BS.hGetContents
    let commit x = mapM_ (\(dn, attrs) ->
            ldapAdd ldap dn attrs) (ldata2LdapMod x)
    commit ltree
    commit lpops

populateTarget :: LDAP -> IO ()
populateTarget ldap = do
    ltree <- withFile "./ldif/target.tree.ldif" ReadMode BS.hGetContents
    lpops <- withFile "./ldif/target.populate.ldif" ReadMode BS.hGetContents
    let commit x = mapM_ (\(dn, attrs) ->
            ldapAdd ldap dn attrs) (ldata2LdapMod x)
    commit ltree
    commit lpops

modifyTreeFromLdif :: LDAP -> BS.ByteString -> IO ()
modifyTreeFromLdif ldap ldif =
    mapM_ (\(LDIFMod dn attrs) ->
            ldapModify ldap dn [attrs]) $
                either (error . show) (id) (parseLDIFStr "" ldif)

ldata2LdapMod :: BS.ByteString -> [(String, [LDAPMod])]
ldata2LdapMod str =
        map (\(LDAPEntry dn attrs) ->
            (dn, list2ldm LdapModAdd attrs)) ldif
    where
        ldif = extractEntries $ parseLDIFStr "" str
        extractEntries x =
            either (error . show) (map (\z -> ldapEntry z)) x

