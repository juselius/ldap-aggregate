--
-- <jonas.juselius@uit.no> 2014
--
module LDIF.Apply (applyLdif) where

import LDIF.Types
import Data.Maybe
import Control.Monad.Error
import Control.Monad.Identity
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S

type ApplyError = ErrorT String Identity

applyLdif :: LDIF -> LDIF -> Either String LDIF
applyLdif mods ldif =
    runIdentity . runErrorT $
      applyLdif' (M.toList mods) ldif

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
    if isNothing $ M.lookup dn ldif
        then return $ M.insert dn a ldif
        else throwError $ "Entry already exists, dn: " ++ dn

delLdif :: LDIF -> Ldif -> ApplyError LDIF
delLdif ldif (dn, _) =
    if isJust $ M.lookup dn ldif
        then return $ M.delete dn ldif
        else throwError $ "Entry does not exists, dn: " ++ dn

modLdif :: LDIF -> Ldif -> ApplyError LDIF
modLdif ldif (dn, a) =
    case M.lookup dn ldif of
        Just e -> do
            a' <- applyEntry a e
            return $ M.insert dn a' ldif
        Nothing -> throwError $ "Entry does not exists! " ++ dn

applyEntry :: LDIFRecord -> LDIFRecord -> ApplyError LDIFRecord
applyEntry (LDIFChange _ m) l = return $ M.foldlWithKey' applyAttr l m
applyEntry _ _ = throwError "Invalid apply!"

applyAttr :: LDIFRecord
          -> Attribute
          -> S.HashSet (LDAPModOp,Value)
          -> LDIFRecord
applyAttr (LDIFEntry dn ldif) name m =
    LDIFEntry dn $
        M.filter S.null $
          M.insert name (S.foldl' applyOp av m) ldif
    where
        av = fromMaybe S.empty $
          M.lookup name ldif
        applyOp acc (op, v) =
            case op of
                LdapModAdd -> S.insert v acc
                LdapModDelete -> S.delete v acc
                LdapModReplace -> S.insert v acc
                _ -> acc
applyAttr ldif _ _ = ldif

isAdd :: Ldif -> Bool
isAdd (_, LDIFEntry _ _) = True
isAdd _ = False

isMod :: Ldif -> Bool
isMod (_, LDIFChange _ _) = True
isMod _ = False

isDel :: Ldif -> Bool
isDel (_, LDIFDelete _) = True
isDel _ = False

