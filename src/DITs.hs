--
-- <jonas.juselius@uit.no> 2014
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module DITs (
      bindLdap
    , printSubTree
    , getSubTree
    , askBindPw
    , commitLdap
    , LDAPEntry(..)
    , LDAPMod(..)
    , DIT(..)
    , SearchBase(..)
    , IgnoreCriterion(..)
    , RewriteCriterion(..)
    ) where

import System.IO
import Data.Yaml
import Data.Maybe
import Data.Monoid
import Control.Applicative
import Control.Monad
import LDAP
import SimpleLDIF
import Aggregate.Edit
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM

data DIT = DIT {
      uri :: T.Text
    , binddn :: T.Text
    , passwd :: T.Text
    , basedn :: T.Text
    , searchBases :: [SearchBase]
    , ignoreFilters :: [IgnoreCriterion]
    , rewriteFilters :: [RewriteCriterion]
    } deriving (Show)

data SearchBase = SearchBase {
      searchBase :: T.Text
    , searchFilter :: T.Text
    } deriving (Show)

newtype IgnoreCriterion = IgnoreCriterion [Criterion Pattern] deriving (Show)
newtype RewriteCriterion = RewriteCriterion [Criterion FromTo] deriving (Show)

instance FromJSON DIT where
    parseJSON (Object o) = DIT
        <$> o .: "uri"
        <*> o .: "binddn"
        <*> o .: "password"
        <*> o .: "base"
        <*> o .: "search"
        <*> o .:? "ignore" .!= mempty
        <*> o .:? "rewrite" .!= mempty
    parseJSON _ = mzero

instance FromJSON SearchBase where
    parseJSON (Object o) = SearchBase
        <$> o .: "basedn"
        <*> o .:? "filter" .!= mempty
    parseJSON _ = mzero

instance FromJSON IgnoreCriterion where
    parseJSON (Object o) = do
        dn    <- o .:? "dn"
        attr  <- o .:? "attr"
        value <- o .:? "value"
        return $ IgnoreCriterion [
              toCriterion (attr `mplus` value) dn
            , toCriterion value attr
            , toCriterion Nothing value
            ]
        where
            toCriterion :: Maybe a -> Maybe Pattern -> Criterion Pattern
            toCriterion p v
                | isJust p = Cont v'
                | otherwise = Break v'
                where v' = fromMaybe mempty v
    parseJSON _ = mzero

noMatch :: (T.Text, T.Text)
noMatch = ("^$", "")

instance FromJSON RewriteCriterion where
    parseJSON (Object o) = do
        dn    <- o `getFromTo` "dn"
        attr  <- o `getFromTo` "attr"
        value <- o `getFromTo` "value"
        return $ RewriteCriterion [
              toCriterion (attr `mplus` value) dn
            , toCriterion value attr
            , toCriterion Nothing value
            ]
        where
            getFromTo x s = fmap parseFromTo (x .:? s)
            toCriterion :: Maybe a -> Maybe FromTo -> Criterion FromTo
            toCriterion p v
                | isJust p = Cont v'
                | otherwise = Break v'
                where v' = fromMaybe noMatch v
            parseFromTo :: Maybe Value -> Maybe FromTo
            parseFromTo (Just (Object x)) =
                flip parseMaybe x $ \y -> (,)
                    <$> y .: "from"
                    <*> y .: "to"
            parseFromTo _ = Nothing

    parseJSON _ = mzero

bindLdap :: DIT-> IO LDAP
bindLdap DIT{..} = do
    ldap <- ldapInitialize uri'
    ldapSimpleBind ldap binddn' passwd'
    return ldap
    where
        uri' = T.unpack uri
        binddn' = T.unpack binddn
        passwd' = T.unpack passwd

getSubTree :: LDAP -> String -> IO [LDAPEntry]
getSubTree ldap tree =
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

commitLdap :: LDAP -> LDIF -> IO ()
commitLdap ldap ldif =
    mapM_ runMod $ HM.toList ldif
    where
        runMod ((T.unpack -> dn), entry) = case entry of
            LDIFAdd    _ (recordToLdapAdd -> e) -> ldapAdd ldap dn e
            LDIFChange _ (recordToLdapMod -> e) -> ldapModify ldap dn e
            LDIFDelete _                        -> ldapDelete ldap dn

printSubTree :: LDAP -> String -> IO ()
printSubTree ldap tree = do
    ldif <- getSubTree ldap tree
    putStrLn . T.unpack . showLdif $ ldapToLdif ldif

