module Block.Hedgehog.Spec.Positional (spec) where

import Block.Class.Positional
import Essentials

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Hedgehog (Gen, forAll, (===), annotateShow)
import Block.Hedgehog.Gen.Positive (positive)
import Prelude ((+))

import qualified Hedgehog.Gen as Gen

spec :: forall x xs.
    (Show x, Eq x) =>
    (Show xs, Eq xs) =>
    (Positional x xs) =>
    Gen x -> Gen xs -> Spec
spec genX genXs = describe "Positional" do

    it "take e (length xs) xs = TakeAll" $ hedgehog do
        xs <- forAll genXs
        e <- forAll Gen.enumBounded

        let l = length xs
        annotateShow l

        take e l xs === TakeAll

    it "take e (length xs + n) xs = TakeInsufficient (Shortfall n)" $ hedgehog do
        xs <- forAll genXs
        e <- forAll Gen.enumBounded

        let l = length xs
        annotateShow l

        n <- forAll (positive 10)

        take e (l + n) xs === TakeInsufficient (Shortfall n)
