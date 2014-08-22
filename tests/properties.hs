import Data.Char
import Test.QuickCheck

main = do
    xmpl
    xmpl'

xmpl = quickCheck ((\s -> (reverse.reverse) s == s) :: [Char] -> Bool)

xmpl' = verboseCheck ((\s -> length s < 3) :: [Integer] -> Bool)
