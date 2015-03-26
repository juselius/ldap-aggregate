--
-- <jonas.juselius@uit.no> 2014
--
{-# LANGUAGE DisambiguateRecordFields #-}

module LDAPAggregate.Types (
      Regexp
    , Subst
    , Culler(..)
) where

type Regexp = String
type Subst = (Regexp, Regexp)

data Culler =
      CullDn {
          cullDn   :: String -> Bool
        }
    | CullAttr {
          cullDn   :: String -> Bool
        , cullAttr :: String -> Bool
        }
    | CullVal {
          cullDn   :: String -> Bool
        , cullAttr :: String -> Bool
        , cullVal  :: String -> Bool
        }
