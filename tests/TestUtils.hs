module TestUtils where

import System.IO
import LDAP
import LDIF.Simple

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
