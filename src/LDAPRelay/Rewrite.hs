--
-- <jonas.juselius@uit.no> 2014
--

module LDAPRelay.Rewrite (
      ldata2LdapMod
    , rewriteDN
    , rewriteAttrs
) where

import LDAP
import LDIF.Simple
import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Text.RegexPR as PR

type RegexFrom = String
type RegexTo = String
type AttrName = String
type FromTo = (RegexFrom, RegexTo)
type AttrFromTo = (AttrName, (RegexFrom, RegexTo))
type AttrList = [AttrName]

ldata2LdapMod :: BS.ByteString -> [(String, [LDAPMod])]
ldata2LdapMod str =
        map (\(LDAPEntry dn attrs) ->
            (dn, list2ldm LdapModAdd attrs)) ldif
    where
        ldif = extractEntries $ parseLDIFStr "" str
        extractEntries =
            either (error . show) (map ldapEntry)

rewriteDN :: [FromTo] -> LDIF -> Maybe LDIF
rewriteDN fts x@(LDIFEntry _ y@(LDAPEntry dn _)) =
    case substDN fts dn of
        Just dn' -> Just $ x { ldapEntry = y {ledn = dn'} }
        Nothing -> Nothing
rewriteDN fts x@(LDIFMod dn _) =
    case substDN fts dn of
        Just dn' -> Just $ x { modDN = dn' }
        Nothing -> Nothing

substDN :: [FromTo] -> String -> Maybe String
substDN fts dn = case getFirstMatch of
        [str] -> Just str
        _ -> Nothing
    where
        getFirstMatch = take 1 . dropWhile (== dn) $
            map (\(src, dst) -> PR.subRegexPR src dst dn) fts

rewriteAttrs :: [AttrFromTo] -> LDIF -> Maybe LDIF
rewriteAttrs afts  x@(LDIFEntry _ y@(LDAPEntry _ attrs)) =
    case Nothing of
        Just _ ->
            Just $ x --{ldapEntry = y {leattrs = attrs'}}
        Nothing -> Nothing
        where
            (update, retained) = partition (\p ->
                isJust $ lookup (fst p) attrs) afts
            updated = []
            result = updated ++ retained
rewriteAttrs afts x@(LDIFMod _ y@(LDAPMod _ attr vals)) =
    case Nothing of
        Just (a, v) ->
            Just $ x {modEntry = y {modType = a, modVals = v }}
        Nothing -> Nothing

filterAttrs :: AttrList -> LDIF -> LDIF
filterAttrs attrl ldif = ldif


