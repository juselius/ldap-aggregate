--
-- <jonas.juselius@uit.no> 2014
--

module LDAPRelay.Types (
      RegexStr
    , FromTo
    , Filter(..)
    , Rewrite(..)
) where

type RegexStr = String
type FromTo = (RegexStr, RegexStr)

data Filter =
      FilterDn     { dnF   :: RegexStr }
    | FilterAttr   { attrF :: RegexStr }
    | FilterDnAttr { dnF   :: RegexStr, attrF:: RegexStr }
    | FilterDnVal  { dnF   :: RegexStr, attrF:: RegexStr, valF :: RegexStr }

data Rewrite =
      RewriteDn   { rwSpec   :: FromTo }
    | RewriteAttr { rwAttr :: String, rwSpec :: FromTo }
