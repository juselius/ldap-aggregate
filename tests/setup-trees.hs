-- | Test ldap conection

module SetupTrees (
      populateSource
    , populateTarget
) where

import System.Environment
import System.IO
import LDAP
import LDIF.Simple
import qualified Data.ByteString.Char8 as BS

import TestUtils

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["source"] -> popS
        ["target"] -> popT
        _ -> popS >> popT
    where
        popS = do
            source <- bindDIT "dc=source"
            populateSource source
            printDIT source "dc=source"
        popT = do
            target <- bindDIT "dc=target"
            populateTarget target
            printDIT target "dc=target"

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
            Right l -> Right $ map (\z -> ldapEntry z) l
            Left err -> Left $ show err

