{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
------------------------------------------------------------------------
-- |
-- Module    : LDIF.Apply
-- Copyright : Jonas Juselius 2014
--
-- Apply modification LDIF (LDIFMod) data to LDIF entries to produce new,
-- updated LDIF entries. In essence, this module implements the core
-- ldapModify behavior, without a server.
------------------------------------------------------------------------
module LDIF.Apply (applyLdif) where

import LDIF.Types
import Data.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Identity
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T

type ApplyError = ExceptT String Identity

applyLdif :: LDIFEntries -> LDIFMods -> Either String LDIFEntries
applyLdif ldif mods =
    runIdentity . runExceptT $ applyMods (HM.elems mods) ldif

applyMods :: [LDIFMod] -> LDIFEntries -> ApplyError LDIFEntries
applyMods mods ldif =
    runAdd ldif >>= runDel >>= runMod
    where
        runAdd = runOp isAdd
        runMod = runOp isMod
        runDel = runOp isDel
        runOp p l = foldM modLdif l (filter p mods)

modLdif :: LDIFEntries -> LDIFMod -> ApplyError LDIFEntries
modLdif ldif = \case
    LDIFAdd dn a  -> tryAdd dn a
    LDIFDelete dn -> tryDel dn
    LDIFChange dn m -> tryChange dn m
    where
        tryAdd dn a =
            if isNothing $ HM.lookup dn ldif
                then return $ HM.insert dn (LDIFRecord dn a) ldif
                else throwExists dn
        tryDel dn =
            if isJust $ HM.lookup dn ldif
                then return $ HM.delete dn ldif
                else throwNotExists dn
        tryChange dn m =
            case HM.lookup dn ldif of
                Just r -> do
                    r'@(LDIFRecord _ a) <- chEntry r m
                    return $ if HM.null a
                        then HM.delete dn ldif
                        else HM.insert dn r' ldif
                Nothing -> throwNotExists dn
        throwExists    dn = throwE $
            "Entry already exists, dn: " ++ T.unpack dn
        throwNotExists dn = throwE $
            "Entry does not exists, dn: " ++ T.unpack dn

chEntry :: LDIFRecord
           -> LDIFAttrs (LDAPModOp, T.Text)
           -> ApplyError LDIFRecord
chEntry le ( m) =
    return $ HM.foldlWithKey' chAttr le m

chAttr :: LDIFRecord
          -> Attr
          -> HS.HashSet (LDAPModOp, Value)
          -> LDIFRecord
chAttr (LDIFRecord dn l) k m =
    LDIFRecord dn $
        -- LDIFAttrs $
            HM.filter (not . HS.null) $
            HM.insert k (HS.foldl' applyOp av m) ldif
    where
        ldif =  l
        av = fromMaybe HS.empty $ HM.lookup k ldif
        applyOp acc (op, v) =
            case op of
                LdapModAdd -> HS.insert v acc
                LdapModDelete -> HS.delete v acc
                LdapModReplace -> HS.insert v acc
                _ -> acc

isAdd :: LDIFMod -> Bool
isAdd (LDIFAdd _ _) = True
isAdd _ = False

isMod :: LDIFMod -> Bool
isMod (LDIFChange _ _) = True
isMod _ = False

isDel :: LDIFMod -> Bool
isDel (LDIFDelete _) = True
isDel _ = False

