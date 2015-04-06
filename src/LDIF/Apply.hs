--
-- <jonas.juselius@uit.no> 2014
--
{-# LANGUAGE OverloadedStrings #-}
module LDIF.Apply (applyLdif) where

import LDIF.Types
import Data.Maybe
import Control.Monad.Error
import Control.Monad.Identity
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T

-- import Debug.Trace

type ApplyError = ErrorT String Identity

applyLdif :: LDIF -> LDIF -> Either String LDIF
applyLdif mods ldif =
    runIdentity . runErrorT $
        applyLdif' (HM.toList mods) ldif

applyLdif' :: [Ldif] -> LDIF -> ApplyError LDIF
applyLdif' mods ldif =
    runAdd ldif >>= runDel >>= runMod
    where
        runAdd = runOp addLdif isAdd
        runMod = runOp modLdif isMod
        runDel = runOp delLdif isDel
        runOp op p l = foldM op l $ filter p mods

addLdif :: LDIF -> Ldif -> ApplyError LDIF
addLdif ldif (dn, a) =
    if isNothing $ HM.lookup dn ldif
        then return $ HM.insert dn a ldif
        else throwError $ "Entry already exists, dn: " ++ T.unpack dn

delLdif :: LDIF -> Ldif -> ApplyError LDIF
delLdif ldif (dn, _) =
    if isJust $ HM.lookup dn ldif
        then return $ HM.delete dn ldif
        else throwError $ "Entry does not exists, dn: " ++ T.unpack dn

modLdif :: LDIF -> Ldif -> ApplyError LDIF
modLdif ldif (dn, m) =
    case HM.lookup dn ldif of
        Just e -> do
            e' <- applyEntry e m
            return $ if HM.null (rAttrs e')
                then HM.delete dn ldif
                else HM.insert dn e' ldif
        Nothing -> throwError $ "Entry does not exists! " ++ T.unpack dn

applyEntry :: LDIFRecord -> LDIFRecord -> ApplyError LDIFRecord
applyEntry le (LDIFChange _ m) = return $ HM.foldlWithKey' applyAttr le m
applyEntry _ _ = throwError "Invalid apply!"

applyAttr :: LDIFRecord
          -> LdifAttr
          -> HS.HashSet (LDAPModOp, LdifValue)
          -> LDIFRecord
applyAttr (LDIFAdd dn ldif) k m =
    LDIFAdd dn $
        HM.filter (not . HS.null) $
        HM.insert k (HS.foldl' applyOp av m) ldif
    where
        av = fromMaybe HS.empty $ HM.lookup k ldif
        applyOp acc (op, v) =
            case op of
                LdapModAdd -> HS.insert v acc
                LdapModDelete -> HS.delete v acc
                LdapModReplace -> HS.insert v acc
                _ -> acc
applyAttr ldif _ _ = ldif

isAdd :: Ldif -> Bool
isAdd (_, LDIFAdd _ _) = True
isAdd _ = False

isMod :: Ldif -> Bool
isMod (_, LDIFChange _ _) = True
isMod _ = False

isDel :: Ldif -> Bool
isDel (_, LDIFDelete _) = True
isDel _ = False

