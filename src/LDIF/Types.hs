module LDIF.Types (
      LDAPModOp(..)
    , LDIFRecord(..)
    , LDIF(..)
    , LDIFContents(..)
    , DN
    , Attribute
    , Value
    , AttrSpec
    , Entry
) where

import LDAP.Modify (LDAPModOp(..))

type DN = String
type Attribute = String
type Value = String
type AttrSpec = (Attribute, [Value])
type Entry = (DN, LDIFRecord)

newtype LDIFContents = LDIFContents [AttrSpec] deriving (Eq)

data LDIFRecord =
      LDIFEntry  { ldifContents :: LDIFContents }
    | LDIFAdd    { ldifContents :: LDIFContents }
    | LDIFDelete
    | LDIFChange {
          ldifOp    :: LDAPModOp
        , ldifMods :: [AttrSpec]
    } deriving (Eq)

newtype LDIF = LDIF { ldifEntry :: Entry } deriving (Eq)

instance Show LDIF where
    show (LDIF (dn, rec)) = case rec of
        x@(LDIFEntry _)    -> formatEntry "LDIFEntry -> " x
        x@(LDIFChange _ _) -> formatEntry "LDIFChange -> " x
        x@(LDIFAdd _)      -> formatEntry "LDIFAdd -> " x
        LDIFDelete         -> "LDIFDelete -> " ++ dn
        where
            formatEntry s x = s ++ "dn: " ++ dn ++ "\n" ++ show x

instance Show LDIFRecord where
    show rec = case rec of
        (LDIFEntry attrs) -> show attrs
        (LDIFAdd attrs) -> show attrs
        (LDIFChange op attrs) -> formatEntry op attrs
        _ -> ""
        where
            formatEntry op attrs = show attrs ++
                "\n    <" ++ show op ++ ">\n"

instance Show LDIFContents where
    show (LDIFContents x) = init . foldl (\s a ->
            s ++ "    " ++ show a ++ "\n") "" $ x
