module Block.Hedgehog.Spec.Singleton (spec) where

import Essentials

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Hedgehog (Gen, forAll, (===))

import qualified Block.Class as Block
import qualified Hedgehog.Gen as Gen

spec :: forall xs x.
    (Block.Singleton xs, Show xs, Eq xs) =>
    (Block.Item xs ~ x, Show x, Eq x) =>
    Gen xs -> Gen (Block.Item xs) -> Spec
spec genBlock genItem = describe "Block.Singleton" do

    it "pop end . unpop end = id" $ hedgehog do
        x <- forAll genItem
        xs <- forAll (Gen.maybe genBlock)
        end <- forAll Gen.enumBounded
        let pop = Block.Pop x xs
        (Block.pop end . Block.unpop end) pop === pop

    it "pop end (singleton x) = Pop x Nothing" $ hedgehog do
        x <- forAll genItem
        end <- forAll Gen.enumBounded
        let xs :: xs = Block.singleton x
        let pop = Block.pop end xs
        pop === Block.Pop x Nothing

    it "pop Front (singleton a <> singleton b) \
       \= Pop a (Just (singleton b))" $ hedgehog do
        a <- forAll genItem
        b <- forAll genItem
        let xs :: xs = Block.singleton a <> Block.singleton b
        let pop = Block.pop Block.Front xs
        pop === Block.Pop a (Just (Block.singleton b))

    it "pop Back (singleton a <> singleton b) \
       \= Pop b (Just (singleton a))" $ hedgehog do
        a <- forAll genItem
        b <- forAll genItem
        let xs :: xs = Block.singleton a <> Block.singleton b
        let pop = Block.pop Block.Back xs
        pop === Block.Pop b (Just (Block.singleton a))
