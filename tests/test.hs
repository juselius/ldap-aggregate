import Test.Tasty
import LdifTests
import RewriteTests
import TestData
import LDIF
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
    --a' <- sampleLdif
    --let
        --a = map entryStr a'
        --l = parseLdif $ BS.pack (unlines a)
        --al = zip a l
    --mapM_ pruit al
    --writeFile "out1" $ unlines a
    --writeFile "out2" $ unlines $ map show l
    --print $ unlines a == (unlines $ map show l)
    defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ldifTests, rewriteTests]
