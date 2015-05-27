--
-- <jonas.juselius@uit.no> 2014
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Aggregate.DITs (
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
import Control.Parallel.Strategies
import Control.Monad
import Control.Monad.Trans.Writer.Strict
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
modifyDIT ldap  lm = do
    mapM_ (runOp "del")    lDel
    mapM_ (runOp "add")    lAdd
    mapM_ (runOp "change") lMod
    where
        ldif = sortLdifByLDn lm
        lDel = toDel (L.reverse ldif)
        lAdd = toAdd ldif
        lMod = toMod ldif
        runOp op (T.unpack -> dn, e) = do
            logDbg 0 (op ++ ": " ++ dn)
            logDbg 1 (info e)
            x <- case op of
                "add"    -> liftIO . try $ ldapAdd    ldap dn e
                "change" -> liftIO . try $ ldapModify ldap dn e
                _        -> liftIO . try $ ldapDelete ldap dn
            either (report e) return x
        info e = unlines (map show e) ++ "--"
        report :: [LDAPMod] -> LDAPException -> Log IO ()
        report l e = logDbg 0 (show e ++ ": >>>\n" ++ show l ++ "<<<")

toDel :: [(T.Text, LDIFMod)] -> [(T.Text, [LDAPMod])]
toDel ldif = map convert $ filter isDel ldif
    where
        isDel (_, LDIFDelete _) = True
        isDel _ = False

toAdd :: [(T.Text, LDIFMod)] -> [(T.Text, [LDAPMod])]
toAdd ldif = r
    where
        r = map convert x
        x = filter isAdd ldif
        isAdd (_, LDIFAdd _ _) = True
        isAdd _ = False

toMod :: [(T.Text, LDIFMod)] -> [(T.Text, [LDAPMod])]
toMod ldif = map convert $ filter isChange ldif
    where
        isChange (_, LDIFChange _ _) = True
        isChange _ = False

convert :: (T.Text, LDIFMod) -> (T.Text, [LDAPMod])
convert (dn, entry) =
    case entry of
        LDIFAdd    _ e -> (dn, recordToLdapAdd e)
        LDIFChange _ e -> (dn, recordToLdapMod e)
        LDIFDelete _   -> (dn, [])

sortLdifByLDn :: LDIFMods -> [(T.Text, LDIFMod)]
sortLdifByLDn ldif =
    L.sortBy (lencomp `on` fst) $ HM.toList ldif
    where
        lencomp a b =  T.length a `compare` T.length b

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
applyLdifRules LDIFRules{..} e =
    withStrategy (parTraversable rdeepseq) $
          reconcileDn
        . runEdits insertRules
        . runEdits rewriteRules
        . runEdits ignoreRules $ e
    where
        -- insert the rewritten dn:s into the hashmap
        reconcileDn = HM.foldl' newDn mempty
        newDn acc v = HM.insert (rDn v) v acc

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
logDbg lvl msg = writer logmsg
    where logmsg = ((),[(lvl, msg)])
