module Block.Hedgehog.Spec.Enumerate (spec) where

import Block.Class.Enumerate
import Essentials

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Hedgehog (Gen, forAll, (===))

import qualified Hedgehog.Gen as Gen
import qualified Block.Hedgehog.Gen.End as Gen

spec :: forall x xs. (Show xs, Enumerate x xs) =>
    Gen xs -> (xs -> Gen xs) -> Spec
spec genXs variegate = describe "Index" do

    it "sameItems = ((==) `on` toNonEmpty)" $ hedgehog do
        a <- forAll genXs
        b <- forAll (Gen.choice [ pure a, variegate a, genXs ])
        end <- forAll Gen.end
        sameItems a b === (toNonEmpty end a == toNonEmpty end b)
