module Block.Hedgehog.Spec.Block (spec) where

import Block.Class
import Essentials

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Hedgehog (Gen, forAll, (===), annotateShow, diff)
import Prelude ((+))
import Block.Hedgehog.Spec.Search (PredicateGenerators (..))

import qualified Block.Hedgehog.Gen.End as Gen
import qualified Block.Hedgehog.Gen.Positive as Gen

spec :: forall x xs. (Eq x, Show x, Show xs, Block x xs) =>
    Gen xs -> (xs -> Gen xs) -> PredicateGenerators x xs -> Spec
spec genXs variegate (PredicateGenerators p _ _) = describe "Block" do

    it "length . toNonEmpty e = length" $ hedgehog do
        xs <- forAll genXs
        end <- forAll Gen.end

        let ne = toNonEmpty end xs
        annotateShow ne

        length ne === length xs

    it "at end n xs = (at end n . toNonEmpty Front)" $ hedgehog do
        xs <- forAll genXs
        end <- forAll Gen.end
        n <- forAll $ Gen.positive $ length xs + 4
        at end n xs === (at end n . toNonEmpty Front) xs

    it "at end n xs = (at Front n . toNonEmpty end)" $ hedgehog do
        xs <- forAll genXs
        end <- forAll Gen.end
        n <- forAll $ Gen.positive $ length xs + 4
        at end n xs === (at Front n . toNonEmpty end) xs

    it "(toNonEmpty end a ++ toNonEmpty end b) \
            \= toNonEmpty end (a ++ b)" $ hedgehog do
        a <- forAll genXs
        b <- forAll genXs
        end <- forAll Gen.end
        ab <- forAll $ variegate $ concat end [a, b]
        (toNonEmpty end a ++ toNonEmpty end b) === toNonEmpty end ab

    it "((fmap . fmap) (toNonEmpty Front) . findPredicate end p) \
            \= (findPredicate end p . toNonEmpty Front)" $ hedgehog do
        xs <- forAll genXs
        end <- forAll Gen.end
        diff
            (((fmap . fmap) (toNonEmpty Front) . findPredicate end p) xs)
            (foldableEqOn sameItemsPivot)
            ((findPredicate end p . toNonEmpty Front) xs)

    it "(fmap (toNonEmpty Front) . spanPredicate end p) \
        \= (spanPredicate end p . toNonEmpty Front)" $ hedgehog do
        xs <- forAll genXs
        end <- forAll Gen.end
        diff
            ((fmap (toNonEmpty Front) . spanPredicate end p) xs)
            sameItemsSpan
            ((spanPredicate end p . toNonEmpty Front) xs)

    it "(fmap (toNonEmpty Front) . take end n) \
            \= (take end n . toNonEmpty Front)" $ hedgehog do
        xs <- forAll genXs
        end <- forAll Gen.end
        n <- forAll $ Gen.positive (length xs + 4)
        diff
            ((fmap (toNonEmpty Front) . take end n) xs)
            sameItemsTake
            ((take end n . toNonEmpty Front) xs)
