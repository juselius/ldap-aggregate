{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO
import LDIF
import LDAP
import LDAPRelay
import Text.Regex.Posix
import Data.Maybe
import Control.Applicative ((<$>))
import Control.Monad (forM_, forever)
import Control.Category ((>>>))
import Data.Configurator as C
import Data.Configurator.Types as C
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
    conf <- C.load [Required "examples/ldaprelay.cfg"]
    auditlog <- C.lookupDefault "examples/auditlog.ldif" conf "auditlog"
    dirs <- fromJust <$> C.lookup conf "source"
    forM_ dirs $ syncDirs . flip C.subconfig conf
    inh <- openFile auditlog ReadMode
    hSeek inh SeekFromEnd 0
    ldap <- bindLdap $ C.subconfig "target" conf
    runUpdates ldap inh
    hClose inh
    BS.putStrLn "done."

syncDirs :: Config -> IO ()
syncDirs conf = do
    omitDn <- getFilter conf "ignore.dn" makeDnFilter
    omitAttrs <- getFilter conf "ignore.attr" makeAttrFilter
    rwDN <- getFilter conf "rewrite.dn" makeRewriteDn
    rwAttrs <- getFilter conf "rewrite.attr" makeRewriteAttrs
    sourceTree <- getDir sc
    targetTree <- getDir tc
    let ldifS = filterDn omitDn
            >>> filterEntries omitAttrs
            >>> rewriteDn rwDN
            >>> rewriteAttrs rwAttrs
            $ sourceTree
        ldifT = filterDn omitDn
            >>> filterEntries omitAttrs
            $ targetTree
    updateDIT tc ldifT $ diffLDIF ldifS ldifT
    where
        sc = C.subconfig "source" conf
        tc = C.subconfig "target" conf

getFilter :: Configured a => Config -> Name -> (a -> b) -> IO [b]
getFilter conf x f = map f <$> fromJust <$> filters
    where
        filters = C.lookup conf x :: Configured a => IO (Maybe [a])

getRewriteDN :: Config -> Name -> IO [FromTo]
getRewriteDN conf x = do
    ft <- (fromJust <$> C.lookup conf x) :: IO [[String]]
    return $ map (\[a, b] -> (a, b)) ft

getRewriteAttrs :: Config -> Name -> IO [(DN, Attribute, FromTo)]
getRewriteAttrs conf x = do
    ft <- (fromJust <$> C.lookup conf x) :: IO [[String]]
    return $ map (\[dn, a, f, t] -> (dn, a, (f, t))) ft

updateDIT :: Config -> [LDIF] -> [LDIF] -> IO ()
updateDIT conf s t = do
    ldap <- bindLdap conf
    runLdif ldap diff
    where
        diff = diffLDIF t s

getDir :: Config -> IO [LDIF]
getDir conf = do
    ldap <- bindLdap conf
    base   <- fromJust <$> C.lookup conf "base"
    ldif <- getDIT ldap base
    return $ map ldapEntry2LDIF ldif

bindLdap :: Config -> IO LDAP
bindLdap conf = do
    uri    <- fromJust <$> C.lookup conf "uri"
    binddn <- fromJust <$> C.lookup conf "binddn"
    passwd <- fromJust <$> C.lookup conf "passwd"
    bindDIT uri binddn passwd

waitForLine :: Handle -> IO BS.ByteString
waitForLine inh = do
    avail <- hWaitForInput inh 50000
    if avail then waitForLine inh else BS.hGetLine inh

matchEmptyLine :: BS.ByteString
matchEmptyLine = "^ *$"

matchBeginRecord :: BS.ByteString
matchBeginRecord = "^# (add|modify|delete) ([0-9]+)"

matchEndRecord :: BS.ByteString
matchEndRecord = "^# end (add|modify|delete) ([0-9]+)"

type Match = (BS.ByteString, BS.ByteString, BS.ByteString, [BS.ByteString])

readAuditlogBlock :: Handle -> [BS.ByteString] -> IO (Integer, BS.ByteString)
readAuditlogBlock inh acc = do
    line <- waitForLine inh
    if line =~ matchEmptyLine
        then readAuditlogBlock inh acc
        else let (_, _, _, hits) = line =~ matchEndRecord :: Match in
            if not $ null hits
                then return (read (BS.unpack $ hits !! 1), BS.unlines acc)
                else readAuditlogBlock inh (acc ++ [line])

runUpdates :: LDAP -> Handle ->  IO ()
runUpdates ldap inh = forever $ do
    ldata <- readAuditlogBlock inh []
    putStrLn $ replicate 50 '-'
    case parseLdifStr [] (snd ldata) of
        Left err -> print err
        Right ldif -> do
            print ldif
            runLdif ldap ldif

