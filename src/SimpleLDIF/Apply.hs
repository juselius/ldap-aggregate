--
-- <jonas.juselius@uit.no> 2014
--
{-# LANGUAGE OverloadedStrings #-}
module SimpleLDIF.Apply (applyLdif) where

import SimpleLDIF.Types
import Data.Maybe
import Control.Monad.Error
import Control.Monad.Identity
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T

type ApplyError = ErrorT String Identity

applyLdif :: LDIF -> LDIF -> Either String LDIF
applyLdif mods ldif =
    runIdentity . runErrorT $
      applyLdif' (HM.toList mods) ldif

applyLdif' :: [Ldif] -> LDIF -> ApplyError LDIF
applyLdif' mods ldif = do
    l1 <- runAdd ldif
    l2 <- runMod l1
    runDel l2
    where
        runAdd = runOp addLdif isAdd
        runDel = runOp delLdif isDel
        runMod = runOp modLdif isMod
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
modLdif ldif (dn, a) =
    case HM.lookup dn ldif of
        Just e -> do
            a' <- applyEntry a e
            return $ HM.insert dn a' ldif
        Nothing -> throwError $ "Entry does not exists! " ++ T.unpack dn

applyEntry :: LDIFRecord -> LDIFRecord -> ApplyError LDIFRecord
applyEntry (LDIFChange _ m) l = return $ HM.foldlWithKey' applyAttr l m
applyEntry _ _ = throwError "Invalid apply!"

applyAttr :: LDIFRecord
          -> LdifAttr
          -> HS.HashSet (LDAPModOp, LdifValue)
          -> LDIFRecord
applyAttr (LDIFAdd dn ldif) name m =
    LDIFAdd dn $
        HM.filter HS.null $
          HM.insert name (HS.foldl' applyOp av m) ldif
    where
        av = fromMaybe HS.empty $
          HM.lookup name ldif
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

