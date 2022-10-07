module Step.Toy.Spec (tests) where

import Step.Toy

import Test.Tasty
import Test.Tasty.Hedgehog

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

tests :: TestTree
tests = testGroup "Toy"
  [
  ]
