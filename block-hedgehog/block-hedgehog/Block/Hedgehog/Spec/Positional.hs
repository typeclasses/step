module Block.Hedgehog.Spec.Positional (spec) where

import Block.Class.Positional
import Essentials

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Hedgehog (Gen, forAll, diff, annotateShow)
import Block.Hedgehog.Gen.Positive (positive)
import Prelude ((+))

import qualified Block.Hedgehog.Gen.End as Gen

spec :: forall xs. (Show xs, Positional xs) =>
    Gen xs -> (xs -> Gen xs) -> Spec
spec genXs variegate = describe "Positional" do

    it "take e (length xs) xs = TakeAll" $ hedgehog do
        xs <- forAll genXs
        e <- forAll Gen.end
        let l = length xs
        annotateShow l
        diff (take e l xs) sameItemsTake TakeAll

    it "take e (length xs + n) xs = TakeInsufficient (Shortfall n)" $ hedgehog do
        xs <- forAll genXs
        e <- forAll Gen.end
        let l = length xs
        annotateShow l
        n <- forAll (positive 10)
        diff (take e (l + n) xs) sameItemsTake (TakeInsufficient (Shortfall n))

    it "take end (length a) (append end a b) = TakePart a b" $ hedgehog do
        a <- forAll genXs
        b <- forAll genXs
        end <- forAll Gen.end
        ab <- forAll $ variegate $ append end a b
        diff (take end (length a) ab) sameItemsTake (TakePart a b)
