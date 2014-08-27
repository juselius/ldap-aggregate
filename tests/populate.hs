-- | Test ldap conection

module Main where

import System.IO
import LDAP
import LDIF.Simple
import Text.Regex.Posix
import qualified Data.ByteString.Char8 as BS

import Debug.Trace
import Text.LDIF.Preproc

main :: IO ()
main = do
    populateSource

populateSource :: IO ()
populateSource = do
    source <- bindDIT "dc=source"
    printDIT source "dc=source"
    putStrLn ""
    ldata <- withFile "./ldif/source.populate.ldif" ReadMode BS.hGetContents
    let ldif = extractEntries $ parseLDIFStr "" ldata
        addldif = map (\(LDAPEntry dn attrs) ->
            (dn, list2ldm LdapModAdd attrs)) ldif
    print ldif
    mapM_ (\(dn, attrs) -> ldapAdd source dn attrs) addldif
    --printDIT source "dc=source"

    --ldapModify ldap "cn=admin,dc=source" [chAdminDesc "Head Foo"]
    --ldif' <- search Nothing LDAPAllUserAttrs False
    --print ldif'
    --where
        --chAdminDesc x = LDAPMod LdapModReplace "description" [x, x ++ "bar"]
    where
        extractEntries x = case x of
            Right l -> map (\z -> entry z) l
            Left err -> error $ show err

printDIT :: LDAP -> String -> IO ()
printDIT ldap tree = do
    ldif <- getDIT ldap tree
    print ldif
    putStrLn $ replicate 50 '+'

bindDIT :: String -> IO LDAP
bindDIT tree = do
    --pw <- getBindPw
    ldap <- ldapInitialize "ldap://localhost"
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
