module Block.Hedgehog.Spec.Search (spec, PredicateGenerators (..)) where

import Block.Class.Search
import Essentials

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Hedgehog (Gen, forAll, (===), annotateShow)

import qualified Data.Maybe as Maybe
import qualified Hedgehog.Gen as Gen
import qualified Data.List.NonEmpty as NonEmpty
import qualified Block.Hedgehog.Gen.End as Gen

data PredicateGenerators x xs =
  PredicateGenerators
    (x -> Bool) -- ^ A predicate
    (Bool -> Gen x) -- ^ Item generators for items that do/don't match the predicate
    (Bool -> Gen xs) -- ^ Block generators for blocks whose items all do/don't match the predicate

spec :: forall x xs.
    (Show x, Eq x) =>
    (Show xs, Eq xs) =>
    (Search x xs) =>
    Gen x -> Gen xs
    -> PredicateGenerators x xs
    -> Spec
spec genX genXs (PredicateGenerators p genX' genXs') = describe "Search" do

    it "find" $ hedgehog do
        a :: Maybe xs <- forAll $ Gen.maybe $ genXs' False
        b :: x        <- forAll $             genX'  True
        c :: Maybe xs <- forAll $ Gen.maybe $ genXs

        end <- forAll Gen.end

        let parts = Maybe.fromJust $ NonEmpty.nonEmpty $ Maybe.catMaybes
                        [a, Just (singleton b) :: Maybe xs, c]

        let abc = concat end parts
        annotateShow abc

        let result = stateless $ find end (\x -> pure $ if p x then Just x else Nothing) abc

        result === Just (Pivot a b c)

    -- todo: test 'find' case where nothing is found

    -- todo: test 'span'
