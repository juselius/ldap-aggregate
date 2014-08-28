-- | Test ldap conection

module Main where

import System.IO
import LDAP
import Text.Regex.Posix

import Debug.Trace

main :: IO ()
main = do
    source <- bindDIT "dc=source"
    target <- bindDIT "dc=target"
    sldif <- getDIT source "dc=source"
    tldif <- getDIT target "dc=target"
    printDIT source "dc=source"
    printDIT target "dc=target"
    putStrLn ""
    let sldif' = reverse $ filter delEntry sldif
    let tldif' = reverse $ filter delEntry tldif
    mapM_ (\dn-> ldapDelete source (ledn dn)) sldif'
    mapM_ (\dn-> ldapDelete target (ledn dn)) tldif'
    printDIT source "dc=source"
    printDIT target "dc=target"

    --ldapModify ldap "cn=admin,dc=source" [chAdminDesc "Head Foo"]
    --ldif' <- search Nothing LDAPAllUserAttrs False
    --print ldif'
    --where
        --chAdminDesc x = LDAPMod LdapModReplace "description" [x, x ++ "bar"]

printDIT :: LDAP -> String -> IO ()
printDIT ldap tree = do
    ldif <- getDIT ldap tree
    print ldif
    putStrLn $ replicate 50 '+'

delEntry :: LDAPEntry -> Bool
delEntry e
    | ledn e =~ "^dc=(source|target)$" :: Bool = False
    | ledn e =~ "^cn=admin,dc=[^=]*$" :: Bool = False
    | otherwise = True

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
