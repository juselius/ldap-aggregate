{- This file is a simplified version of Text.LDIF.Preproc. -}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO
import Control.Concurrent
import LDIF
import LDAP
import LDAPRelay
import Text.Regex.Posix
--import Settings
import Data.Maybe
import Control.Applicative ((<$>))
import Control.Monad (forM_, forever)
import Data.Configurator as C
import Data.Configurator.Types as C
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
    conf <- C.load [Required "examples/ldaprelay.cfg"]
    auditlog <- C.lookupDefault "examples/auditlog.ldif" conf "auditlog"
    dirs <- fromJust <$> C.lookup conf "directories"
    forM_ dirs $ syncDirs . flip C.subconfig conf
    inh <- openFile auditlog ReadMode
    hSeek inh SeekFromEnd 0
    ldap <- bindLdap $ C.subconfig "target" conf
    runUpdates ldap inh
    hClose inh
    BS.putStrLn "done."

syncDirs :: Config -> IO ()
syncDirs conf = do
    ignDN <- getFromTo conf "ignore.dn"
    ignAttrs <- getFromTo conf "ignore.attr"
    rwDN <- getFromTo conf "rewrite.dn"
    rwAttrs <- getFromTo conf "rewrite.attr"
    sourceLdif <- getDir sc
        -- >>= filterDN ignDN
        -- >>= filterAttrs ignAttrs
        -- >>= rewriteDN rwDN
        -- >>= rewriteAttrs rwDN
    targetLdif <- getDir tc
    updateDIT tc targetLdif $ diffLDIF sourceLdif targetLdif
    where
        sc = C.subconfig "source" conf
        tc = C.subconfig "target" conf

getFromTo :: Config -> Name -> IO [FromTo]
getFromTo conf x = do
    ft <- (fromJust <$> C.lookup conf x) :: IO [[String]]
    return $ map (\[a,b] -> (a,b)) ft

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

