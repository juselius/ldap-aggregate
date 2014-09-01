module Settings (
      readSettings
    , Settings(..)
    , DITInfo(..)
    ) where

import System.Exit
import Data.ConfigFile.Monadic
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
        cp <- join . liftIO $ readfile f dp
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

readDIT :: ConfigParser -> String -> ErrorT CPError IO DITInfo
readDIT cp sect = do
    url <- get cp sect "uri"
    bas <- get cp sect "base"
    dn <- get cp sect "binddn"
    pw <- get cp sect "password"
    return $ DITInfo url bas dn pw

defaultCP :: ErrorT CPError IO ConfigParser
defaultCP =
    set "DEFAULT" "auditlog" "auditlog.ldif" emptyCP
    >>= add_section "source"
    >>= set "source" "uri" "ldap://localhost:389"
    >>= set "source" "binddn" ""
    >>= set "source" "base" ""
    >>= set "source" "secret" ""
    >>= set "source" "binddn" ""
    >>= add_section "dest"
    >>= set "dest" "uri" "ldap://localhost:389"
    >>= set "dest" "binddn" ""
    >>= set "dest" "base" ""
    >>= set "dest" "secret" ""
    >>= set "dest" "binddn" ""
    >>= add_section "rewrite"
    >>= set "rewrite" "dn" "[]"
    >>= set "rewrite" "attr" "[]"

