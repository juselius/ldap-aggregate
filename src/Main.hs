{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import System.IO
import Control.Concurrent
{-import Text.Regex.PCRE-}
import Settings

main = do
    settings <- readSettings "ldaprelay.cfg"
    print settings
    inh <- openFile "input.txt" ReadMode
    processData inh
    hClose inh
    putStrLn "done."

waitForLine :: Handle -> IO String
waitForLine inh = do
    atEof <- hIsEOF inh
    if atEof
        then do
            threadDelay 50000
            waitForLine inh
        else hGetLine inh

processData :: Handle ->  IO ()
processData inh = do
    str <- waitForLine inh
    putStrLn str
    processData inh

