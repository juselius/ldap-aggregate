{-# LANGUAGE OverloadedStrings #-}
module EditTests (
    editTests
) where

import Test.Tasty
import Test.Tasty.HUnit
import Editor
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T

import Debug.Trace

editTests :: TestTree
editTests = testGroup "Editor" [
      testCase "filter 0" $ testF0 @?= True
    , testCase "filter 1" $ testF1 @?= True
    , testCase "filter 2" $ testF2 @?= True
    , testCase "rewrite 0" $ testR0 @?= True
    , testCase "rewrite 1" $ testR1 @?= True
    , testCase "rewrite 2" $ testR2 @?= True
    ]

testF0 :: Bool
testF0 = edit f ldif == ref
    where
        f :: [Criterion T.Text]
        f = [Break "A"]
        ref = HM.fromList [
            ("B", HM.fromList [
              ("b1", HS.fromList ["b1-1","b1-2"])
            , ("b2", HS.fromList ["b2-1","b2-2"])
            ]
            )]

testF1 :: Bool
testF1 = edit f ldif == ref
    where
        f :: [Criterion T.Text]
        f = [Cont "A", Break "a1"]
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
        f :: [Criterion T.Text]
        f = [Cont "A", Cont "a1", Break "a1-1"]
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
        rw :: [Criterion FromTo]
        rw = [Break ("A", "C")]
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
        rw :: [Criterion FromTo]
        rw = [Cont ("A", "C"), Break ("a([0-9])", "c\\1")]
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
testR2 = trace (show (foldl (flip edit) ldif rw)) foldl (flip edit) ldif rw == ref
    where
        rw :: [[Criterion FromTo]]
        rw = [
              [Cont ("A", "C"), Cont ("a([0-9])", "c\\1"), Break ("a1-", "c1-")]
            , [Cont ("(.*)", "\\1"), Cont ("(.*)", "\\1"), Break ("a2-", "c2-")]
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


