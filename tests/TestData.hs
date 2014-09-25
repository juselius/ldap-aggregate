module TestData where

import LDIF
import Control.Arrow (first)
import qualified Data.ByteString.Char8 as BS

genLdif' :: String -> [AttrSpec] -> BS.ByteString
genLdif' dn av = BS.pack $ "dn: " ++ dn ++ "\n" ++ attrs
    where
        attrs = foldl (\s (a, v) ->
            s ++ a ++ ": " ++ v ++ "\n") "" (expanded av)
        expanded = concat . map (uncurry zip . first repeat)

genLdif :: String -> [AttrSpec] -> LDIF
genLdif dn av = LDIF (dn, LDIFEntry $ LDAPEntry dn av)


ldifDef1 :: [AttrSpec]
ldifDef1 = [
      ("A", ["A1", "A2"])
    , ("B", ["B1"])
    ]

ldifDef2 :: [AttrSpec]
ldifDef2 = [
      ("A", ["A1"])
    ]

ldiff1 :: [LDIF]
ldiff1 = [
      LDIF ("B", LDIFAdd [LDAPMod LdapModAdd "A" ["A1"]])
    , LDIF ("A", LDIFDelete)
    ]

ldiff2 :: [LDIF]
ldiff2 = [
      LDIF ("A", LDIFAdd [ LDAPMod LdapModAdd "A" ["A1", "A2"]
                         , LDAPMod LdapModAdd "B" ["B1"]
                         ]
           )
    , LDIF ("B", LDIFDelete)
    ]

