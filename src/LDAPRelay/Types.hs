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
      FilterDn        { dnF   :: RegexStr }
    | FilterDnAttr    { dnF   :: RegexStr, attrF:: String }
    | FilterDnAttrVal { dnF   :: RegexStr, attrF:: String, valF :: String }
    | FilterAttr      { attrF :: String }

data Rewrite =
      RewriteDn   { rwSpec   :: FromTo }
    | RewriteAttr { rwAttr :: String, rwSpec :: FromTo }
