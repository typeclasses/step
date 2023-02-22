module Block.Hedgehog.Spec.NonEmptyIso (spec) where

import Block.Class.NonEmptyIso
import Essentials

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Hedgehog (Gen, forAll, (===), annotateShow)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Block.Hedgehog.Gen.End as Gen

spec :: forall x xs.
    (Show x, Eq x) =>
    (Show xs, Eq xs) =>
    (NonEmptyIso x xs) =>
    Gen x -> Gen xs -> Spec
spec genX genXs = describe "NonEmptyIso" do

    it "toNonEmpty e . fromNonEmpty e = id" $ hedgehog do
        ne <- forAll (Gen.nonEmpty (Range.linear 1 20) genX)
        e <- forAll Gen.end

        let xs :: xs = fromNonEmpty e ne
        annotateShow xs

        toNonEmpty e xs === ne

    it "toNonEmpty e . fromNonEmpty e . toNonEmpty e = toNonEmpty e" $ hedgehog do
        xs <- forAll genXs
        e <- forAll Gen.end

        let ne = toNonEmpty e xs
        annotateShow ne

        let xs' :: xs = fromNonEmpty e ne
        annotateShow xs'

        toNonEmpty e xs' === ne
