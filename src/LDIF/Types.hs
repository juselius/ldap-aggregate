{-# LANGUAGE LambdaCase #-}
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
    , isContentsLdif
    , extractContentsLdif
    , toAList
) where

import LDAP.Modify (LDAPModOp(..))
import Control.Arrow(second)

type DN = String
type Attribute = String
type Value = String
type AttrSpec = (Attribute, [Value])
type Entry = (DN, LDIFRecord)

data LDIFRecord =
      LDIFEntry  { ldifRecord :: [AttrSpec] }
    | LDIFAdd    { ldifRecord :: [AttrSpec] }
    | LDIFChange { ldifOp :: LDAPModOp , ldifRecord :: [AttrSpec] }
    | LDIFDelete
    deriving (Eq)

newtype LDIF = LDIF { ldifEntry :: Entry } deriving (Eq)

newtype LDIFContents = LDIFContents (DN, [AttrSpec]) deriving (Eq)

isContentsLdif :: LDIFRecord -> Bool
isContentsLdif = \case
    LDIFEntry _ -> True
    LDIFAdd _ -> True
    _ -> False

extractContentsLdif :: [LDIF] -> [LDIFContents]
extractContentsLdif l = map (\(LDIF x) ->
    LDIFContents (second ldifRecord x)) $ selectContents l
    where
        selectContents = filter (\(LDIF (_, rec)) -> isContentsLdif rec)

toAList :: [LDIFContents] -> [(DN, [AttrSpec])]
toAList = map (\(LDIFContents x) -> x)

instance Show LDIF where
    show (LDIF (dn, rec)) = case rec of
        x@(LDIFEntry _)    -> formatEntry "LDIFEntry -> " x
        x@(LDIFChange _ _) -> formatEntry "LDIFChange -> " x
        x@(LDIFAdd _)      -> formatEntry "LDIFAdd -> " x
        LDIFDelete         -> "LDIFDelete -> " ++ dn
        where
            formatEntry s x = s ++ "dn: " ++ dn ++ "\n" ++ show x

instance Show LDIFRecord where
    show = \case
        (LDIFEntry attrs) -> show attrs
        (LDIFAdd attrs) -> show attrs
        (LDIFChange op attrs) -> formatEntry op attrs
        _ -> ""
        where
            formatEntry op attrs = show attrs ++
                "\n    <" ++ show op ++ ">\n"

instance Show LDIFContents where
    show (LDIFContents (dn, x)) = "LDIFContents -> dn: " ++ dn ++ "\n" ++
        show x

