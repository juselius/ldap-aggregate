--
-- <jonas.juselius@uit.no> 2014
--
module LDIF.Apply (
    applyLdif
) where

import LDIF.Types
import Data.Maybe
import Data.List
import Control.Monad.Error
import Control.Monad.Identity

type ApplyError = ErrorT String Identity
type Ldif = (DN, LDIFRecord)

applyLdif :: [LDIF] -> [LDIF] -> Either String [LDIF]
applyLdif mods ldif = runIdentity . runErrorT $
    applyLdif' ops ldif
    where
        ops = map ldifEntry2Add mods

applyLdif' :: [Ldif] -> [Ldif] -> ApplyError [Ldif]
applyLdif' mods ldif = do
    l1 <- runAdd ldif
    l2 <- runMod l1
    runDel l2
    where
        runAdd = runOp addLdif isAdd
        runDel = runOp delLdif isDel
        runMod = runOp modLdif isMod
        runOp op p l = foldM op l $ filter p mods

addLdif :: [Ldif] -> Ldif -> ApplyError [Ldif]
addLdif ldif (dn, a) =
    if isNothing $ lookup dn ldif
    then return $ (dn, LDIFEntry (fromJust $ ldifRecord2Entry dn a)):ldif
    else throwError $ "Entry already exists, dn: " ++ dn

delLdif :: [Ldif] -> Ldif -> ApplyError [Ldif]
delLdif ldif m@(dn, _) =
    if isJust $ lookup dn ldif
    then return $ deleteBy cmpfst m ldif
    else throwError $ "Entry does not exists, dn: " ++ dn

modLdif :: [Ldif] -> Ldif -> ApplyError [Ldif]
modLdif ldif m@(dn, a) =
    case lookup dn ldif of
        Just e -> do
            modfy <- applyEntry a e
            return $ modfy : deleteBy cmpfst m ldif
        Nothing -> throwError $ "Entry does not exists! " ++ dn

applyEntry :: LDIFRecord -> LDIFRecord -> ApplyError Ldif
applyEntry (LDIFChange m) (LDIFEntry (LDAPEntry dn l)) = do
    attrs <- foldM applyAttr l m
    return (dn, LDIFEntry $ LDAPEntry dn attrs)
applyEntry (LDIFChange _) _ = throwError "Cannot apply LDAPMod to LDAPMod!"
applyEntry (LDIFEntry _) _ = throwError "Cannot apply LDAPEntry!"
applyEntry _ _ = throwError "Invalid apply!"

applyAttr :: [AttrSpec] -> LDAPMod -> ApplyError [AttrSpec]
applyAttr attrs (LDAPMod op name vals) = return $
    case op of
        LdapModAdd -> maybe addAttr appendAttr (lookup name attrs)
        LdapModDelete -> maybe attrs delAttr (lookup name attrs)
        LdapModReplace -> replaceAttr
        _ -> attrs
        where
            addAttr = (name, vals):attrs
            appendAttr v = (name, vals ++ v):rest
            delAttr v = if null (delv v) then rest else (name, delv v):rest
            replaceAttr = (name, vals):rest
            rest = deleteBy cmpfst (name, []) attrs
            delv v = foldl' (\acc x -> deleteBy (==) x acc) v vals

isAdd :: Ldif -> Bool
isAdd (_, LDIFAdd _) = True
isAdd _ = False

isDel :: Ldif -> Bool
isDel (_, LDIFDelete) = True
isDel _ = False

isMod :: Ldif -> Bool
isMod (_, LDIFChange _) = True
isMod _ = False

cmpfst :: Eq a => (a, b) -> (a, b) -> Bool
cmpfst (a, _) (b, _) = a == b
