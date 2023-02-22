module Block.Hedgehog.Spec (spec, refinedSpec) where

import Essentials
import Block.Class

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Hedgehog (Gen, forAll, (===), annotateShow)

import qualified Block.Hedgehog.Spec.Singleton as Singleton
import qualified Block.Hedgehog.Spec.Positional as Positional
import qualified Block.Hedgehog.Spec.NonEmptyIso as NonEmptyIso
import qualified Block.Hedgehog.Spec.Search as Search
import qualified Block.Hedgehog.Spec.Refined as Refined
import qualified Hedgehog.Gen as Gen

spec :: forall x xs.
    (Show x, Eq x) =>
    (Show xs, Eq xs) =>
    (
      Semigroup xs, Singleton x xs, Positional x xs,
      NonEmptyIso x xs, Search x xs
    ) =>
    Gen x -> Gen xs -> Spec
spec genX genXs = describe "Block" do
    Singleton.spec genX genXs
    Positional.spec genX genXs
    NonEmptyIso.spec genX genXs
    Search.spec genX genXs

    it "length . toNonEmpty e = length" $ hedgehog do
        xs <- forAll genXs
        e <- forAll Gen.enumBounded

        let ne = toNonEmpty e xs
        annotateShow ne

        length ne === length xs

    it "pop Front (singleton a <> singleton b) \
       \= Pop a (Just (singleton b))" $ hedgehog do
        a <- forAll genX
        b <- forAll genX
        let xs :: xs = singleton a <> singleton b
        let p = pop Front xs
        p === Pop a (Just (singleton b))

    it "pop Back (singleton a <> singleton b) \
       \= Pop b (Just (singleton a))" $ hedgehog do
        a <- forAll genX
        b <- forAll genX
        let xs :: xs = singleton a <> singleton b
        let p = pop Back xs
        p === Pop b (Just (singleton a))

refinedSpec :: forall x nul xs.
    (Show x, Eq x) =>
    (Show nul, Eq nul) =>
    (Show xs, Eq xs) =>
    (
      Semigroup xs, Monoid nul, Singleton x xs, Positional x xs,
      NonEmptyIso x xs, Search x xs, Refined x nul xs
    ) =>
    Gen x -> Gen nul -> Gen xs -> Spec
refinedSpec x nul xs = do
    spec x xs
    Refined.spec x nul xs
