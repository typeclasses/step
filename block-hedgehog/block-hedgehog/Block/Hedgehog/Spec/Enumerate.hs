module Block.Hedgehog.Spec.Enumerate (spec) where

import Block.Class
import Essentials

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Hedgehog (Gen, forAll, (===))

import qualified Hedgehog.Gen as Gen
import qualified Block.Hedgehog.Gen.End as Gen
import qualified Data.List.NonEmpty as NonEmpty
import qualified Fold.ShortcutNonempty as Fold

spec :: forall x xs. (Eq x, ItemEquality xs, Show x, Show xs, Enumerate x xs) =>
    Gen xs -> (xs -> Gen xs) -> Spec
spec genXs variegate = describe "Enumerate" do

    it "sameItems = ((==) `on` toNonEmpty end)" $ hedgehog do
        a <- forAll genXs
        b <- forAll (Gen.choice [ pure a, variegate a, genXs ])
        end <- forAll Gen.end
        sameItems a b === (toNonEmpty end a == toNonEmpty end b)

    it "(reverse . toNonEmpty end) = toNonEmpty (opposite end)" $ hedgehog do
        xs <- forAll genXs
        end <- forAll Gen.end
        (NonEmpty.reverse . toNonEmpty end) xs === toNonEmpty (oppositeEnd end) xs

    it "toNonEmpty end = foldItems end (motivate list)" $ hedgehog do
        xs <- forAll genXs
        end <- forAll Gen.end
        toNonEmpty end xs === foldItems end (Fold.motivate Fold.list) xs
