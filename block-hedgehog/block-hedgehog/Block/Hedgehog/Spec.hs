module Block.Hedgehog.Spec (spec, refinedSpec, PredicateGenerators (..)) where

import Essentials
import Block.Class

import Test.Hspec (Spec, it)
import Test.Hspec.Hedgehog (hedgehog)
import Hedgehog (Gen, forAll, (===), annotateShow)
import Block.Hedgehog.Spec.Search (PredicateGenerators (..))

import qualified Block.Hedgehog.Spec.Concat as Concat
import qualified Block.Hedgehog.Spec.Enumerate as Enumerate
import qualified Block.Hedgehog.Spec.Index as Index
import qualified Block.Hedgehog.Spec.Singleton as Singleton
import qualified Block.Hedgehog.Spec.Positional as Positional
import qualified Block.Hedgehog.Spec.NonEmptyIso as NonEmptyIso
import qualified Block.Hedgehog.Spec.Search as Search
import qualified Block.Hedgehog.Spec.Refined as Refined
import qualified Hedgehog.Gen as Gen

spec :: forall x xs. (Show x, Show xs, NonEmptyIso x xs, Search x xs, Index x xs) =>
    Gen x -> Gen xs -> (xs -> Gen xs) -> PredicateGenerators x xs -> Spec
spec genX genXs variegate genP  = do
    Concat.spec genXs
    Singleton.spec genX genXs variegate
    Enumerate.spec genXs variegate
    Positional.spec genXs variegate
    NonEmptyIso.spec genX genXs variegate
    Search.spec genXs variegate genP
    Index.spec genX genXs variegate

    it "length . toNonEmpty e = length" $ hedgehog do
        xs <- forAll genXs
        e <- forAll Gen.enumBounded

        let ne = toNonEmpty e xs
        annotateShow ne

        length ne === length xs

refinedSpec :: forall x nul xs. (Show x, Show nul, Show xs, Eq nul, Eq xs,
        NonEmptyIso x xs, Search x xs, Index x xs, Refined nul xs) =>
    Gen x -> Gen nul -> Gen xs -> (xs -> Gen xs) -> PredicateGenerators x xs -> Spec
refinedSpec genX genNul genXs variegate genP = do
    spec genX genXs variegate genP
    Refined.spec genNul genXs
