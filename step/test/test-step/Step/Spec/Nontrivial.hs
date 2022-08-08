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
import qualified Step.Nontrivial.ListLike.Construction as LL

import Char (Char)

import Step.RST

import Positive.Unsafe

tests :: TestTree
tests = testGroup "Nontrivial"
  [ dropTests
  ]

dropTests :: TestTree
dropTests = testGroup "drop"
  [ testCase "all" (drop (PositiveUnsafe 1) (LL.nontrivialUnsafe ("abc" :: Text))
      @?= DropPart{ dropRemainder = LL.nontrivialUnsafe "bc" })
  ]
  where
    DropOperation{ drop } = LL.dropOperation
