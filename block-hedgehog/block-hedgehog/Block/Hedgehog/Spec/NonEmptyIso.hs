module Block.Hedgehog.Spec.NonEmptyIso (spec) where

import Block.Class
import Essentials

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Hedgehog (Gen, forAll, (===), diff, annotateShow)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Block.Hedgehog.Gen.End as Gen

spec :: forall x xs. (Eq x, ItemEquality xs, Show x, Show xs, Block x xs) =>
    Gen x -> Gen xs -> (xs -> Gen xs) -> Spec
spec genX genXs variegate = describe "NonEmptyIso" do

    it "toNonEmpty e . fromNonEmpty e = id" $ hedgehog do
        ne <- forAll $ Gen.nonEmpty (Range.linear 1 20) genX
        e <- forAll Gen.end
        xs :: xs <- forAll $ variegate $ fromNonEmpty e ne
        toNonEmpty e xs === ne

    it "fromNonEmpty e . toNonEmpty e = id" $ hedgehog do
        xs <- forAll genXs
        e <- forAll Gen.end
        let ne = toNonEmpty e xs
        annotateShow ne
        diff (fromNonEmpty e ne) sameItems xs
