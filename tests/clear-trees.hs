-- | Test ldap conection

module ClearTree (
      clearTree
    , clearTree'
) where

import System.Environment
import System.IO
import LDAP
import Text.Regex.Posix

import TestUtils
{-import Debug.Trace-}

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["source"] -> doS
        ["target"] -> doT
        _ -> doS >> doT
    where
        doS = clearTree' "dc=source"
        doT = clearTree' "dc=target"

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

    --ldapModify ldap "cn=admin,dc=source" [chAdminDesc "Head Foo"]
    --ldif' <- search Nothing LDAPAllUserAttrs False
    --print ldif'
    --where
        --chAdminDesc x = LDAPMod LdapModReplace "description" [x, x ++ "bar"]

delEntry :: LDAPEntry -> Bool
delEntry e
    | ledn e =~ "^dc=(source|target)$" :: Bool = False
    | ledn e =~ "^cn=admin,dc=[^=]*$" :: Bool = False
    | otherwise = True

