module Block.Hedgehog.Spec.Search (spec, PredicateGenerators (..)) where

import Block.Class.Search
import Essentials

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Hedgehog (Gen, forAll, diff)

import qualified Data.Maybe as Maybe
import qualified Hedgehog.Gen as Gen
import qualified Data.List.NonEmpty as NonEmpty
import qualified Block.Hedgehog.Gen.End as Gen

data PredicateGenerators x xs =
  PredicateGenerators
    (x -> Bool) -- ^ A predicate
    (Bool -> Gen x) -- ^ Item generators for items that do/don't match the predicate
    (Bool -> Gen xs) -- ^ Block generators for blocks whose items all do/don't match the predicate

spec :: forall x xs. (Show x, Eq x, Show xs, Search x xs) =>
    Gen xs
    -> (xs -> Gen xs)
    -> PredicateGenerators x xs
    -> Spec
spec genXs variegate (PredicateGenerators p genX' genXs') = describe "Search" do

    it "findPredicate -> Just" $ hedgehog do
        a :: Maybe xs <- forAll $ Gen.maybe $ genXs' False
        b :: x        <- forAll $ genX' True
        c :: Maybe xs <- forAll $ Gen.maybe $ genXs

        end <- forAll Gen.end

        let parts = Maybe.fromJust $ NonEmpty.nonEmpty $ Maybe.catMaybes
                        [a, Just (singleton b) :: Maybe xs, c]

        abc :: xs <- forAll $ variegate $ concat end parts

        diff (findPredicate end p abc) (foldableEqOn sameItemsPivot) (Just (Pivot a b c))

    it "findPredicate -> Nothing" $ hedgehog do
        x <- forAll $ genXs' False
        end <- forAll Gen.end
        diff (findPredicate end p x) (foldableEqOn sameItemsPivot) Nothing

    it "spanPredicate -> SpanPart" $ hedgehog do
        a :: xs       <- forAll $ genXs' True
        b :: x        <- forAll $ genX' False
        c :: Maybe xs <- forAll $ Gen.maybe $ genXs

        end <- forAll Gen.end

        let parts = Maybe.fromJust $ NonEmpty.nonEmpty $ Maybe.catMaybes
                        [Just a, Just (singleton b) :: Maybe xs, c]

        abc :: xs <- forAll $ variegate $ concat end parts

        diff (spanPredicate end p abc) sameItemsSpan (SpanPart a (unpop end (Pop b c)))

    it "spanPredicate -> SpanAll" $ hedgehog do
        x <- forAll $ genXs' True
        end <- forAll Gen.end
        diff (spanPredicate end p x) sameItemsSpan SpanAll

    it "spanPredicate -> SpanNone" $ hedgehog do
        x <- forAll $ genXs' False
        end <- forAll Gen.end
        diff (spanPredicate end p x) sameItemsSpan SpanNone
