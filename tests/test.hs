import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import LdifTests
import RewriteTests

main :: IO ()
main = do
    foop
   --defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, ldifTests, rewriteTests]

properties :: TestTree
properties = testGroup "Properties"
    [ QC.testProperty "example 1" xmpl ]

xmpl :: [Char] -> Bool
xmpl s = reverse (reverse s) == s

