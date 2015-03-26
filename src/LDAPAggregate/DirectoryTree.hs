--
-- <jonas.juselius@uit.no> 2014
--
module LDAPAggregate.DirectoryTree (
      bindDIT
    , printDIT
    , getDIT
    , askBindPw
    , runLdif
    , LDAPEntry(..)
    , LDAPMod(..)
    ) where

import System.IO
import LDAP
import LDIF

printDIT :: LDAP -> String -> IO ()
printDIT ldap tree = do
    ldif <- getDIT ldap tree
    putStrLn . init . foldl prettify  "" $ ldif
    putStrLn "--"
    where
        prettify s e@(LDAPEntry dn _) =
            showLdif (dn, LDIFAdd dn e) ++ "\n" ++ s

bindDIT :: String -> String -> String -> IO LDAP
bindDIT uri binddn pw = do
    ldap <- ldapInitialize uri
    ldapSimpleBind ldap binddn pw
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

runLdif :: LDAP -> [LDIF] -> IO ()
runLdif ldap =
    mapM_ runMod
    where
        runMod (dn, entry) = case entry of
            LDIFAdd _ e -> ldapAdd ldap dn e
            LDIFChange _ e -> ldapModify ldap dn e
            LDIFDelete _ -> ldapDelete ldap dn
