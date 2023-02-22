module Block.Hedgehog.Spec.Refined (spec) where

import Block.Class.Refined
import Essentials

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Hedgehog (Gen, forAll, (===))

import qualified Hedgehog.Gen as Gen

spec :: forall nul xs.
    (Show xs, Eq xs) =>
    (Refined nul xs) =>
    Gen nul -> Gen xs -> Spec
spec genNul genXs = describe "Refined" do
    pure ()
