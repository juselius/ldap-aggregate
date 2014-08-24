{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO
import Control.Concurrent
import Text.LDIF.Parser
import qualified Data.ByteString.Char8 as BS
import Text.Regex.Posix
import Settings

main = do
    settings <- readSettings "ldaprelay.cfg"
    inh <- openFile (auditlog settings) ReadMode
    processData inh
    hClose inh
    BS.putStrLn "done."

waitForLine :: Handle -> IO BS.ByteString
waitForLine inh = do
    atEof <- hIsEOF inh
    if atEof
        then do
            threadDelay 50000
            waitForLine inh
        else BS.hGetLine inh

isEmptyLine :: BS.ByteString -> Bool
isEmptyLine line =
    line =~ BS.pack "^ *$" :: Bool

isBeginAuditLog :: BS.ByteString -> Bool
isBeginAuditLog line =
    line =~ BS.pack "^# (add|modify|delete) ([0-9]+)" :: Bool

readAuditlogBlock :: Handle -> [BS.ByteString] -> IO BS.ByteString
readAuditlogBlock inh acc = do
    line <- waitForLine inh
    if isEmptyLine line || isBeginAuditLog line
        then readAuditlogBlock inh acc
        else if line =~
            BS.pack "^# end (add|modify|delete) ([0-9]+)" :: Bool
            then return $ BS.unlines acc
            else readAuditlogBlock inh (acc ++ [line])

processData :: Handle ->  IO ()
processData inh = do
    ldata <- readAuditlogBlock inh []
    case parseLDIFStr defaulLDIFConf "relay" ldata of
        Left err -> print "err"
        Right ldif -> print ldif
    processData inh

