--
-- <jonas.juselius@uit.no> 2014
--
{-# LANGUAGE OverloadedStrings #-}
module Auditlog (
  monitorAuditlog
) where

import System.IO
import Text.Regex.Posix
import Data.Maybe
import LDIF
import LDAPAggregate
import qualified Data.ByteString.Char8 as BS

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

monitorAuditlog :: LDAP -> Handle ->  IO ()
monitorAuditlog ldap inh = forever $ do
    ldata <- readAuditlogBlock inh []
    putStrLn $ replicate 50 '-'
    case parseLdifStr [] (snd ldata) of
        Left err -> print err
        Right ldif -> do
            print ldif
            commitLdif ldap ldif
