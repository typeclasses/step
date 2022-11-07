module Main (main) where

import Test.Tasty

import qualified Step.Spec.Characters
import qualified Step.Spec.FixedLength
import qualified Step.Spec.Match
import qualified Step.Spec.ParticularText

main = defaultMain tests

tests :: TestTree
tests = testGroup "Step"
  [ Step.Spec.Characters.tests
  , Step.Spec.FixedLength.tests
  , Step.Spec.Match.tests
  , Step.Spec.ParticularText.tests
  ]
