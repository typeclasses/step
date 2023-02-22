module Block.Hedgehog.Spec.Refined (spec) where

import Block.Class.Refined
import Essentials

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Hedgehog (Gen, forAll, (===))

import qualified Hedgehog.Gen as Gen

spec :: forall x nul xs.
    (Show x, Eq x) =>
    (Show xs, Eq xs) =>
    (Refined x nul xs) =>
    Gen x -> Gen nul -> Gen xs -> Spec
spec genX genNul genXs = describe "Refined" do
    pure ()
