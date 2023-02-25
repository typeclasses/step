module Block.Hedgehog.Spec.Block (spec) where

import Block.Class.Block
import Essentials

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Hedgehog (Gen, forAll, (===), annotateShow)

import qualified Hedgehog.Gen as Gen
import qualified Block.Hedgehog.Gen.End as Gen

spec :: forall x xs. (Show x, Show xs, Block x xs) =>
    Gen xs -> (xs -> Gen xs) -> Spec
spec genXs variegate = describe "Block" do

    it "length . toNonEmpty e = length" $ hedgehog do
        xs <- forAll genXs
        end <- forAll Gen.end

        let ne = toNonEmpty end xs
        annotateShow ne

        length ne === length xs
