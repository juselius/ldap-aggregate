{-# LANGUAGE OverloadedStrings #-}
module EditTests (
    editTests
) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Monoid
import LDIF.Editor
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T

-- import Debug.Trace

editTests :: TestTree
editTests = testGroup "Editor" [
      testCase "filter 0" $ testF0 @?= True
    , testCase "filter 1" $ testF1 @?= True
    , testCase "filter 2" $ testF2 @?= True
    , testCase "rewrite 0" $ testR0 @?= True
    , testCase "rewrite 1" $ testR1 @?= True
    , testCase "rewrite 2" $ testR2 @?= True
    , testCase "rewrite 3" $ testR3 @?= True
    , testCase "monoid 0" $ testM0 @?= True
    , testCase "monoid 1" $ testM1 @?= True
    ]

ldif :: HM.HashMap T.Text (HM.HashMap T.Text (HS.HashSet T.Text))
ldif = HM.fromList [
      ("A", HM.fromList [
          ("a1", HS.fromList ["a1-1", "a1-2"])
        , ("a2", HS.fromList ["a2-1", "a2-2"])
        ])
    , ("B", HM.fromList [
          ("b1", HS.fromList ["b1-1","b1-2"])
        , ("b2", HS.fromList ["b2-1","b2-2"])
        ]
    )]

testF0 :: Bool
testF0 = edit f ldif == ref
    where
        f :: Rule T.Text
        f = Delete "A" Done
        ref = HM.fromList [
            ("B", HM.fromList [
              ("b1", HS.fromList ["b1-1","b1-2"])
            , ("b2", HS.fromList ["b2-1","b2-2"])
            ]
            )]

testF1 :: Bool
testF1 = edit f ldif == ref
    where
        f :: Rule T.Text
        f = Delete "A" $ Delete "a1" Done
        ref = HM.fromList [
              ("A", HM.fromList [
                  ("a2", HS.fromList ["a2-1", "a2-2"])
                ])
            , ("B", HM.fromList [
                  ("b1", HS.fromList ["b1-1","b1-2"])
                , ("b2", HS.fromList ["b2-1","b2-2"])
                ]
            )]

testF2 :: Bool
testF2 = edit f ldif == ref
    where
        f :: Rule T.Text
        f = Delete "A" $ Delete "a1" $ Delete "a1-1" Done
        ref = HM.fromList [
              ("A", HM.fromList [
                  ("a1", HS.fromList ["a1-2"])
                , ("a2", HS.fromList ["a2-1", "a2-2"])
                ])
            , ("B", HM.fromList [
                  ("b1", HS.fromList ["b1-1","b1-2"])
                , ("b2", HS.fromList ["b2-1","b2-2"])
                ]
            )]

testR0 :: Bool
testR0 = edit rw ldif == ref
    where
        rw :: Rule T.Text
        rw = Subst "A" "C" Done
        ref = HM.fromList [
              ("C", HM.fromList [
                  ("a1", HS.fromList ["a1-1", "a1-2"])
                , ("a2", HS.fromList ["a2-1", "a2-2"])
                ])
            , ("B", HM.fromList [
                  ("b1", HS.fromList ["b1-1","b1-2"])
                , ("b2", HS.fromList ["b2-1","b2-2"])
                ]
            )]

testR1 :: Bool
testR1 = edit rw ldif == ref
    where
        rw :: Rule T.Text
        rw = Subst "A" "C" $ Subst "a([0-9])" "c\\1" Done
        ref = HM.fromList [
              ("C", HM.fromList [
                  ("c1", HS.fromList ["a1-1", "a1-2"])
                , ("c2", HS.fromList ["a2-1", "a2-2"])
                ])
            , ("B", HM.fromList [
                  ("b1", HS.fromList ["b1-1","b1-2"])
                , ("b2", HS.fromList ["b2-1","b2-2"])
                ]
            )]

testR2 :: Bool
testR2 = foldl (flip edit) ldif rw == ref
    where
        rw :: [Rule T.Text]
        rw = [
              Subst "A" "C" $ Subst "a([0-9])" "c\\1" $ Subst "a1-" "c1-" Done
            , Subst "(.*)" "\\1" $ Subst "(.*)" "\\1" $ Subst "a2-" "c2-" Done
            ]
        ref = HM.fromList [
              ("C", HM.fromList [
                  ("c1", HS.fromList ["c1-1", "c1-2"])
                , ("c2", HS.fromList ["c2-1", "c2-2"])
                ])
            , ("B", HM.fromList [
                  ("b1", HS.fromList ["b1-1","b1-2"])
                , ("b2", HS.fromList ["b2-1","b2-2"])
                ]
            )]

testR3 :: Bool
testR3 = foldl (flip edit) ldif rw == ref
    where
        rw :: [Rule T.Text]
        rw = [
              Subst "A" "" $ Subst "(.*)" "\\1" Done
            , Subst "(.*)" "\\1" $ Subst "(.*)" "\\1" $ Subst ".*" "" Done
            ]
        ref = HM.empty

testM0 :: Bool
testM0 = Done <> m <> mempty == m
    where m = Delete T.empty Done

testM1 :: Bool
testM1 = m0 <> m1 <> mempty == ref
    where
        m0  = Delete 0 $ Delete 1 Done :: Rule Int
        m1  = Delete 2 $ Delete 3 Done :: Rule Int
        ref = Delete 0 $ Delete 1 $ Delete 2 $ Delete 3 Done :: Rule Int

