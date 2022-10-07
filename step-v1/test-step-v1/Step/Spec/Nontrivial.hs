{-# language FlexibleContexts, OverloadedStrings #-}

module Step.Spec.Nontrivial where

import Step.Internal.Prelude

import Hedgehog
import qualified Hedgehog.Gen as Gen

import Test.Tasty
import Test.Tasty.HUnit ((@?=), testCase)

import Text (Text)

import Loc (loc)

import qualified Map

import Step.Nontrivial
import qualified Step.Nontrivial.ListLike as LL

import Char (Char)

import Step.RST

import Positive.Unsafe

import Maybe (fromJust)

tests :: TestTree
tests = testGroup "Nontrivial"
  [ dropTests
  ]

dropTests :: TestTree
dropTests = testGroup "drop"
  [ testCase "part" $
      drop (PositiveUnsafe 1) (fromJust (untrivialize ("abc" :: Text)))
        @?= DropPart{ dropRemainder = fromJust (untrivialize "bc") }
  , testCase "all" $
      drop (PositiveUnsafe 3) (fromJust (untrivialize ("abc" :: Text)))
        @?= DropAll
  , testCase "insufficient" $
      drop (PositiveUnsafe 4) (fromJust (untrivialize ("abc" :: Text)))
        @?= DropInsufficient{ dropShortfall = PositiveUnsafe 1 }
  ]
  where
    DropOperation{ drop } = LL.dropOperation
    UntrivializeOperation{ untrivialize } = LL.untrivializeOperation
