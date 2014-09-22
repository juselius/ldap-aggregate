module LDAPRelay.DirectoryTree (
      bindDIT
    , printDIT
    , getDIT
    , askBindPw
    , modifyTreeFromLdif
    , modifyTreeFromLdifStr
    , LDAPEntry(..)
    , LDAPMod(..)
    ) where

import System.IO
import LDAP
import LDIF.Parser
import qualified Data.ByteString.Char8 as BS

printDIT :: LDAP -> String -> IO ()
printDIT ldap tree = do
    ldif <- getDIT ldap tree
    putStrLn . init . foldl prettify  "" $ ldif
    putStrLn "--"
    where
        prettify s (LDAPEntry dn attrs) =
            show (LDIF (dn, LDIFEntry attrs)) ++ "\n" ++ s

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

modifyTreeFromLdifStr :: LDAP -> BS.ByteString -> IO ()
modifyTreeFromLdifStr ldap ldif =
    modifyTreeFromLdif ldap $ either (error . show) id (parseLDIFStr "" ldif)

modifyTreeFromLdif :: LDAP -> [LDIF] -> IO ()
modifyTreeFromLdif ldap ldif =
    mapM_ runMod ldif
    where
        runMod (LDIF (dn, entry)) = case entry of
            LDIFEntry attrs ->
                ldapAdd ldap dn (list2ldm LdapModAdd attrs)
            LDIFChange LdapModAdd attrs ->
                ldapAdd ldap dn (list2ldm LdapModAdd attrs)
            LDIFChange LdapModDelete _ ->
                ldapDelete ldap dn
            LDIFUpdate op attrs ->
                ldapModify ldap dn (list2ldm op attrs)
            _ -> error "Invalid LDIF operation."
