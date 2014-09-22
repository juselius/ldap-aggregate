{- This file is a simplified version of Text.LDIF.Preproc. -}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO
import Control.Concurrent
{-import Text.LDIF.Preproc-}
import LDIF.Parser
import qualified Data.ByteString.Char8 as BS
import Text.Regex.Posix

import Settings

main :: IO ()
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
                then return $ (read (BS.unpack $ hits !! 1), BS.unlines acc)
                else readAuditlogBlock inh (acc ++ [line])

processData :: Handle ->  IO ()
processData inh = do
    ldata <- readAuditlogBlock inh []
    putStrLn $ replicate 50 '-'
    case parseLDIFStr "relay" (snd ldata) of
        Left err -> print err
        Right ldif -> print ldif
    processData inh

