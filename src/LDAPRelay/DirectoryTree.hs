module LDAPRelay.DirectoryTree (
      bindDIT
    , printDIT
    , getDIT
    , askBindPw
    , modifyTreeFromLdif
    , LDAPEntry(..)
    , LDAPMod(..)
    ) where

import System.IO
import LDAP
import LDIF.Simple
import qualified Data.ByteString.Char8 as BS

printDIT :: LDAP -> String -> IO ()
printDIT ldap tree = do
    ldif <- getDIT ldap tree
    putStrLn . init . foldl prettify  "" $ ldif
    putStrLn "--"
    where
        prettify s (LDAPEntry dn attrs) =
            show (LDIFEntry LdapModAdd dn attrs) ++ "\n" ++ s

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
    mapM_ doMod $ either (error . show) id (parseLDIFStr "" ldif)
    where
        doMod (LDIF (LDIFEntry LdapModAdd dn attrs)) =
            ldapAdd ldap dn (list2ldm LdapModAdd attrs)
        doMod (LDIF (LDIFEntry LdapModDelete dn _)) =
            ldapDelete ldap dn
        doMod (LDIF (LDIFEntry op dn attrs)) =
            ldapModify ldap dn (list2ldm op attrs)
        doMod (LDIFMod (LDIFEntry op dn attrs)) =
            ldapModify ldap dn (list2ldm op attrs)
