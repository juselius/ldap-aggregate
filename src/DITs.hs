--
-- <jonas.juselius@uit.no> 2014
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module DITs (
      bindDIT
    , modifyDIT
    , fetchTree
    , fetchSubTree
    , applyLdifRules
    , getLdifRules
    , updateTimeStamp
    , getCurrentTimeStamp
    , printSubTree
    , logDbg
    , LDAPEntry(..)
    , LDAPMod(..)
    , DIT(..)
    , SearchBase(..)
    , LDIFRules(..)
    , Log
    ) where

import Data.Yaml
import Data.Function
import Data.Time.Clock
import Data.Time.Format
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Writer.Lazy
import Control.Monad.IO.Class
import LDAP
import LDIF
import LDIF.Editor
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.HashMap.Lazy as HM

data DIT = DIT {
      uri :: T.Text
    , binddn :: T.Text
    , passwd :: T.Text
    , searchBases    :: [SearchBase]
    , ignoreFilters  :: [IgnoreRule]
    , rewriteFilters :: [RewriteRule]
    } deriving (Show, Eq)

data SearchBase = SearchBase {
      searchBase :: T.Text
    , searchFilter :: T.Text
    } deriving (Show, Eq, Ord)

data LDIFRules = LDIFRules {
      rewriteRules :: [Rule T.Text]
    , ignoreRules  :: [Rule T.Text]
    , insertRules  :: [Rule T.Text]
    } deriving (Show)

type Log a b = WriterT [(Int, String)] a b

instance FromJSON DIT where
    parseJSON (Object o) = fmap addAttrRewriteDn $ DIT
        <$> o .: "uri"
        <*> o .: "binddn"
        <*> o .: "password"
        <*> o .: "search"
        <*> o .:? "ignore" .!= mempty
        <*> o .:? "rewrite" .!= mempty
    parseJSON _ = mzero

instance FromJSON SearchBase where
    parseJSON (Object o) = SearchBase
        <$> o .: "basedn"
        <*> o .:? "filter" .!= "objectClass=*"
    parseJSON _ = mzero

bindDIT :: DIT -> IO LDAP
bindDIT DIT{..} = do
    ldap <- ldapInitialize uri'
    ldapSimpleBind ldap binddn' passwd'
    return ldap
    where
        uri' = T.unpack uri
        binddn' = T.unpack binddn
        passwd' = T.unpack passwd

modifyDIT :: LDAP -> LDIFMods -> Log IO ()
modifyDIT ldap ldif = do
    mapM_ runMod lDel
    mapM_ runMod lOther
    where
        ldif' = L.sortBy (orf `on` fst) $ HM.toList ldif
        (L.reverse -> lDel, lOther) = L.partition (pf . snd) ldif'
        orf a b =  T.length a `compare` T.length b
        runMod (T.unpack -> dn, entry) = case entry of
            LDIFAdd    _ (recordToLdapAdd -> e) -> do
                logDbg 0 ("add: " ++ dn)
                logDbg 1 (info e)
                x <- liftIO . try $ ldapAdd ldap dn e
                either (report entry) return x
            LDIFChange _ (recordToLdapMod -> e) -> do
                unless (null e) (logDbg 0 ("change: " ++ dn))
                unless (null e) (logDbg 1 (info e))
                x <- liftIO . try $ ldapModify ldap dn e
                either (report entry) return x
            LDIFDelete _                        -> do
                logDbg 0 ("delete: " ++ dn)
                x <- liftIO . try $ ldapDelete ldap dn
                either (report entry) return x
        pf (LDIFDelete _) = True
        pf _ = False
        info e = unlines (map show e) ++ "--"
        report :: LDIFMod -> LDAPException -> Log IO ()
        report l e = logDbg 0 (show e ++ ": >>>\n" ++ show l ++ "<<<")

fetchTree :: LDAP -> [SearchBase] -> IO [LDAPEntry]
fetchTree ldap = foldM (\a b -> fmap (a ++) (fetchSubTree ldap b)) []

fetchSubTree :: LDAP -> SearchBase -> IO [LDAPEntry]
fetchSubTree ldap SearchBase{..} =
    ldapSearch ldap (Just base) LdapScopeSubtree
        (Just filt) LDAPAllUserAttrs False
        where
            base = T.unpack searchBase
            filt = T.unpack searchFilter

printSubTree :: LDAP -> SearchBase -> IO ()
printSubTree ldap tree = do
    ldif <- fetchSubTree ldap tree
    print $ ldapToLdif ldif

getLdifRules :: DIT -> LDIFRules
getLdifRules DIT{..} = LDIFRules rw ign ins
    where
        rw  = map doRewrite rewriteFilters
        ign = map doIgnore  ignoreFilters
        ins = []

applyLdifRules :: LDIFRules -> LDIFEntries -> LDIFEntries
applyLdifRules LDIFRules{..} =
      reconcileDn
    . runEdits insertRules
    . runEdits rewriteRules
    . runEdits ignoreRules
    where
        -- insert the rewritten dn:s into the hashmap
        reconcileDn = HM.foldl' (\acc v -> HM.insert (rDn v) v acc) mempty

-- for every dn rewrite, add the corresponding rewrite to attr dn
addAttrRewriteDn :: DIT -> DIT
addAttrRewriteDn d@DIT{..} =
    d { rewriteFilters = foldl df mempty rewriteFilters }
    where
        df acc r@(RewriteRule (Subst f t _)) =
            RewriteRule (Cont ".*" (Subst f t Done)):r:acc
        df acc r = r:acc

-- | Update new timestamp to all search bases
updateTimeStamp :: T.Text -> DIT -> DIT
updateTimeStamp ts d@DIT{..} =
    d { searchBases = map (addModifyTimestamp ts) searchBases }

addModifyTimestamp :: T.Text -> SearchBase -> SearchBase
addModifyTimestamp ts b@SearchBase{..} = b {
        searchFilter = addTSFilter
        }
    where
        addTSFilter =
            "(&(modifyTimestamp>=" `T.append` ts `T.append` ")"
            `T.append` sf `T.append` ")"
        sf =
            if T.head searchFilter == '('
                then searchFilter
                else "(" `T.append` searchFilter `T.append` ")"

getCurrentTimeStamp :: IO T.Text
getCurrentTimeStamp = tsfmt <$> getCurrentTime
    where
        tsfmt = T.pack . formatTime defaultTimeLocale "%Y%m%d%H%M%SZ"

logDbg :: Monad m => Int -> String -> WriterT [(Int, String)] m ()
logDbg lvl msg = writer ((),[(lvl, msg)])
