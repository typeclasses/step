module Block.Hedgehog.Spec.Singleton (spec) where

import Block.Class.Singleton
import Essentials

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Hedgehog (Gen, forAll, diff)

import qualified Hedgehog.Gen as Gen
import qualified Block.Hedgehog.Gen.End as Gen

spec :: forall x xs. (Show x, Show xs, Singleton x xs) =>
    Gen x -> Gen xs -> (xs -> Gen xs) -> Spec
spec genX genXs variegate = describe "Singleton" do

    it "pop end . unpop end = id" $ hedgehog do
        x <- forAll genX
        xs <- forAll (Gen.maybe genXs)
        end <- forAll Gen.end
        let p = Pop x xs
        xs' <- forAll $ variegate $ unpop end p
        diff (pop end xs') sameItemsPop p

    it "pop end (singleton x) = Pop x Nothing" $ hedgehog do
        x <- forAll genX
        end <- forAll Gen.end
        let xs :: xs = singleton x
        diff (pop end xs) sameItemsPop (Pop x Nothing)

    it "pop end (push end x xs) = Pop x (Just xs)" $ hedgehog do
        x <- forAll genX
        xs <- forAll genXs
        end <- forAll Gen.end
        diff (pop end (push end x xs)) sameItemsPop (Pop x (Just xs))

    it "pop Front (singleton a ++ singleton b) \
       \= Pop a (Just (singleton b))" $ hedgehog do
        a <- forAll genX
        b <- forAll genX
        xs :: xs <- forAll $ variegate $ singleton a ++ singleton b
        diff (pop Front xs) sameItemsPop (Pop a (Just (singleton b)))

    it "pop Back (singleton a ++ singleton b) \
       \= Pop b (Just (singleton a))" $ hedgehog do
        a <- forAll genX
        b <- forAll genX
        xs :: xs <- forAll $ variegate $ singleton a ++ singleton b
        diff (pop Back xs) sameItemsPop (Pop b (Just (singleton a)))
