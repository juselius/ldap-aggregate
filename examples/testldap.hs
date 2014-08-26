-- | Test ldap conection

module Main where

import System.IO
import LDAP

main :: IO ()
main = do
    putStr "bindpw: "
    hFlush stdout
    pw <- getLine
    putStrLn ""
    ldap <- ldapInitialize "ldap://localhost"
    ldapSimpleBind ldap "cn=admin,dc=source" pw
    let search = ldapSearch ldap (Just "dc=source") LdapScopeSubtree
    ldif <- search Nothing LDAPAllUserAttrs False
    print ldif
    ldapModify ldap "cn=admin,dc=source" [chAdminDesc "Head Foo"]
    putStrLn $ replicate 50 '+'
    ldif' <- search Nothing LDAPAllUserAttrs False
    print ldif'
    where
        chAdminDesc x = LDAPMod LdapModReplace "description" [x, x ++ "bar"]
