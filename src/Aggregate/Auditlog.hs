{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------
-- |
-- Module    : Aggregate.Auditlog
-- Copyright : Jonas Juselius 2014
--
-- Read and parse an LDAP auditlog stream in LDIF format.
------------------------------------------------------------------------
module Aggregate.Auditlog (
  monitorAuditlog
) where

import System.IO
import Text.Regex.TDFA
import Data.Maybe
import Control.Monad
import LDIF
import LDIF.Editor
import LDAP
import qualified Data.Text as T
import qualified Data.Text.IO as T

waitForLine :: Handle -> IO T.Text
waitForLine inh = do
    avail <- hWaitForInput inh 50000
    if avail then waitForLine inh else T.hGetLine inh

matchEmptyLine :: String
matchEmptyLine = "^ *$"

matchBeginRecord :: String
matchBeginRecord = "^# (add|modify|delete) ([0-9]+)"

matchEndRecord :: String
matchEndRecord = "^# end (add|modify|delete) ([0-9]+)"

type Match = (String, String, String, [String])

readAuditlogBlock :: Handle -> [T.Text] -> IO (Integer, T.Text)
readAuditlogBlock inh acc = do
    line <- waitForLine inh
    if T.unpack line =~ matchEmptyLine
        then readAuditlogBlock inh acc
        else let (_, _, _, hits) = T.unpack line =~ matchEndRecord :: Match in
            if not $ null hits
                then return (read (hits !! 1), T.unlines acc)
                else readAuditlogBlock inh (acc ++ [line])

monitorAuditlog :: LDAP -> Handle ->  IO ()
monitorAuditlog ldap inh = forever $ do
    ldata <- readAuditlogBlock inh []
    putStrLn $ replicate 50 '-'
    case parseLdif (snd ldata) of
        Left err -> print err
        Right ldif -> do
            print ldif
            commitLdap ldap ldif
