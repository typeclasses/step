module Block.Hedgehog.Spec.Index (spec) where

import Block.Class.Index
import Block.Class.ItemEquality
import Essentials

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Hedgehog (Gen, forAll, (===))
import Prelude ((+))

import qualified Block.Class.End as End
import qualified Block.Hedgehog.Gen.End as Gen

spec :: forall x xs.
    (Show x, Eq x) =>
    (Show xs, ItemEquality xs) =>
    (Index x xs) =>
    Gen x -> Gen xs -> (xs -> Gen xs) -> Spec
spec genX genXs variegate = describe "Index" do

    it "at end 1 . singleton = Just" $ hedgehog do
        x <- forAll genX
        end <- forAll Gen.end
        (at end 1 . singleton @x @xs) x === Just x

    it "at end 1 = Just . terminal end" $ hedgehog do
        xs <- forAll genXs
        end <- forAll Gen.end

        at end 1 xs === (Just . terminal end) xs

    it "at end (length xs) xs = Just (terminal (opposite end) xs)" $ hedgehog do
        xs <- forAll genXs
        end <- forAll Gen.end

        at end (length xs) xs === Just (terminal (End.opposite end) xs)

    it "at end (length xs + 1) xs = Nothing" $ hedgehog do
        xs <- forAll genXs
        end <- forAll Gen.end

        at end (length xs + 1) xs === Nothing

    it "at/terminal/++" $ hedgehog do
        a <- forAll genXs
        b <- forAll genXs

        ab <- forAll $ variegate (a ++ b)

        at Front (length a)     ab === Just (last a)
        at Back  (length b + 1) ab === Just (last a)
        at Front (length a + 1) ab === Just (first b)
        at Back  (length b)     ab === Just (first b)
