module Block.Hedgehog.Spec.Singleton (spec) where

import Block.Class.Singleton
import Essentials

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Hedgehog (Gen, forAll, (===))

import qualified Hedgehog.Gen as Gen
import qualified Block.Hedgehog.Gen.End as Gen

spec :: forall x xs.
    (Show x, Eq x) =>
    (Show xs, Eq xs) =>
    (Singleton x xs) =>
    Gen x -> Gen xs -> Spec
spec genX genXs = describe "Singleton" do

    it "pop end . unpop end = id" $ hedgehog do
        x <- forAll genX
        xs <- forAll (Gen.maybe genXs)
        end <- forAll Gen.end

        let p = Pop x xs

        (pop end . unpop end) p === p

    it "pop end (singleton x) = Pop x Nothing" $ hedgehog do
        x <- forAll genX
        end <- forAll Gen.end

        let xs :: xs = singleton x
        let p = pop end xs

        p === Pop x Nothing

    it "pop end (push end x xs) = Pop x (Just xs)" $ hedgehog do
        x <- forAll genX
        xs <- forAll genXs
        end <- forAll Gen.end

        pop end (push end x xs) === Pop x (Just xs)
