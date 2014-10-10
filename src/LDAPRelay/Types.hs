--
-- <jonas.juselius@uit.no> 2014
--

module LDAPRelay.Types (
      RegexStr
    , FromTo
    , Filter(..)
) where

type RegexStr = String
type FromTo = (RegexStr, RegexStr)

data Filter a =
      FilterDn      { operand   :: a }
    | FilterAttr    { operand   :: a }
    | FilterVal     { matchAttr :: RegexStr, operand :: a }
    | FilterAttrDn  { matchDn   :: RegexStr, operand :: a }
    | FilterValDn   { matchDn   :: RegexStr
                    , matchAttr :: RegexStr
                    , operand   :: a
                    }
    | InvalidFilter

