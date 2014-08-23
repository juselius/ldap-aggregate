module Settings (
      readSettings
    , Settings(..)
    , DITInfo(..)
    ) where

import System.Exit
import Data.ConfigFile
import Control.Monad.Error

data Settings = Settings {
      source :: DITInfo
    , dest :: DITInfo
    , auditlog :: FilePath
    , rewriteDNs :: [(String, String)]
    , rewriteAttrs :: [(String, String, String)]
    } deriving (Show)

data DITInfo = DITInfo {
        uri :: String
      , base :: String
      , binddn :: String
      , password :: String
    } deriving (Show)

readSettings :: String -> IO Settings
readSettings f = do
    s <- runErrorT $ do
        dp <- defaultCP
        cp <- join . liftIO $ readfile dp f
        src <- readDIT cp "source"
        dst <- readDIT cp "dest"
        rdn <- get cp "rewrite" "dn"
        ratt <- get cp "rewrite" "attr"
        alog <- get cp "DEFAULT" "auditlog"
        return Settings {
                  auditlog = alog
                , source = src
                , dest = dst
                , rewriteDNs = read rdn :: [(String, String)]
                , rewriteAttrs = read ratt :: [(String, String, String)]
            }
    returnOrFail s
    where
        returnOrFail s = case s of
            Right s' -> return s'
            Left s' -> do
                putStrLn "Error reading config file:"
                putStrLn $ " ---> " ++ show s'
                exitFailure

-- use applicative style!
readDIT :: ConfigParser -> String -> ErrorT CPError IO DITInfo
readDIT cp sect = do
    uri <- get cp sect "uri"
    base <- get cp sect "base"
    binddn <- get cp sect "binddn"
    pw <- get cp sect "password"
    return $ DITInfo uri base binddn pw

defaultCP :: ErrorT CPError IO ConfigParser
defaultCP = do
    let cp = emptyCP
    cp <- set cp "DEFAULT" "auditlog" "auditlog.ldif"
    cp <- add_section cp "source"
    cp <- set cp "source" "uri" "ldap://localhost:389"
    cp <- set cp "source" "binddn" ""
    cp <- set cp "source" "base" ""
    cp <- set cp "source" "secret" ""
    cp <- set cp "source" "binddn" ""
    cp <- add_section cp "dest"
    cp <- set cp "dest" "uri" "ldap://localhost:389"
    cp <- set cp "dest" "binddn" ""
    cp <- set cp "dest" "base" ""
    cp <- set cp "dest" "secret" ""
    cp <- set cp "dest" "binddn" ""
    cp <- add_section cp "rewrite"
    cp <- set cp "rewrite" "dn" "[]"
    cp <- set cp "rewrite" "attr" "[]"
    return cp

