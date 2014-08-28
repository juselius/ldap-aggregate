-- | Test ldap conection

module Main where

import System.IO
import LDAP
import LDIF.Simple
import Text.Regex.Posix
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
    populateSource

populateSource :: IO ()
populateSource = do
    ltree <- withFile "./ldif/source.tree.ldif" ReadMode BS.hGetContents
    lpops <- withFile "./ldif/source.populate.ldif" ReadMode BS.hGetContents
    source <- bindDIT "dc=source"
    let commit x = mapM_ (\(dn, attrs) ->
            ldapAdd source dn attrs) (ldata2LdapMod x)
    print $ ldata2LdapMod ltree
    --commit ltree
    --commit lpops
    printDIT source "dc=source"

    --ldapModify ldap "cn=admin,dc=source" [chAdminDesc "Head Foo"]
    --ldif' <- search Nothing LDAPAllUserAttrs False
    --print ldif'
    --where
        --chAdminDesc x = LDAPMod LdapModReplace "description" [x, x ++ "bar"]

ldata2LdapMod :: BS.ByteString -> [(String, [LDAPMod])]
ldata2LdapMod str =
    case ldif of
        Right l -> map (\(LDAPEntry dn attrs) ->
            (dn, list2ldm LdapModAdd attrs)) l
        Left e -> error e
    where
        ldif = extractEntries $ parseLDIFStr "" str
        extractEntries x = case x of
            Right l -> Right $ map (\z -> entry z) l
            Left err -> Left $ show err

printDIT :: LDAP -> String -> IO ()
printDIT ldap tree = do
    ldif <- getDIT ldap tree
    print ldif
    putStrLn $ replicate 50 '+'

bindDIT :: String -> IO LDAP
bindDIT tree = do
    --pw <- getBindPw
    ldap <- ldapInitialize "ldap://localhost:1389"
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
