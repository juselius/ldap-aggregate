module LDIF.Apply (
    applyLDIF
) where

import LDIF.Types
import Data.Maybe
import Data.List
import Control.Monad.Error
import Control.Monad.Identity

type ApplyError = ErrorT String Identity
type Ldif = (DN, LDIFRecord)

applyLDIF :: [LDIF] -> [LDIF] -> Either String [LDIF]
applyLDIF mods ldif = runIdentity . runErrorT $
    liftM toLDIF $ applyLDIF' ops (fromLDIF ldif)
    where
        ops = fromLDIF $ map ldif2mod mods

applyLDIF' :: [Ldif] -> [Ldif] -> ApplyError [Ldif]
applyLDIF' mods ldif = runAdd >>= runMod >>= runDel
    where
        runAdd = mapM (`addLdif` ldif) $ filter isAdd mods
        runDel = delLdif (filter isDel mods)
        runMod l = mapM (`modLdif` l) $ filter isMod mods

addLdif :: Ldif -> [Ldif] -> ApplyError Ldif
addLdif (dn, a) l =
    if isNothing $ lookup dn l
    then return (dn, LDIFEntry (fromJust $ record2entry dn a))
    else throwError $ "Entry already exists! " ++ dn

delLdif :: [Ldif] -> [Ldif] -> ApplyError [Ldif]
delLdif d l = return . foldr rmMatchDn l $ d
    where
        rmMatchDn = deleteBy cmpfst
        cmpfst (a, _) (b, _) = a == b

modLdif :: Ldif -> [Ldif] -> ApplyError Ldif
modLdif (dn, a) l =
    case lookup dn l of
        Just e -> applyEntry a e
        Nothing -> throwError $ "Entry does not exists! " ++ dn

applyEntry :: LDIFRecord -> LDIFRecord -> ApplyError Ldif
applyEntry (LDIFChange m) (LDIFEntry (LDAPEntry dn l)) = do
    attrs <- foldM (\acc x-> applyAttr x acc) l m
    return (dn, LDIFEntry $ LDAPEntry dn attrs)
applyEntry (LDIFChange _) _ = throwError "Cannot apply LDAPMod to LDAPMod!"
applyEntry (LDIFEntry _) _ = throwError "Cannot apply LDAPEntry!"

applyAttr :: LDAPMod -> [AttrSpec] -> ApplyError [AttrSpec]
applyAttr (LDAPMod LdapModAdd name vals) attrs = return attrs
applyAttr (LDAPMod LdapModDelete name vals) attrs = return attrs
applyAttr (LDAPMod LdapModReplace name vals) attrs = return attrs

isAdd :: Ldif -> Bool
isAdd (_, LDIFAdd _) = True
isAdd _ = False

isDel :: Ldif -> Bool
isDel (_, LDIFDelete) = True
isDel _ = False

isMod :: Ldif -> Bool
isMod (_, LDIFChange _) = True
isMod _ = False
