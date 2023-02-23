module Block.Hedgehog.Spec.Refined (spec) where

import Block.Class.Refined
import Block.Class.ItemEquality
import Essentials

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Hedgehog (Gen, forAll, (===), diff)

spec :: forall nul xs.
    (Show xs, ItemEquality xs) =>
    (Show nul, Eq nul) =>
    (Refined nul xs) =>
    Gen nul -> Gen xs -> Spec
spec genNul genXs = describe "Refined" do

    it "refine . generalize = Just" $ hedgehog do
        xs <- forAll genXs

        diff ((refine . generalize) xs) (foldableEqOn sameItems) (Just xs)

    it "if (refine a = Just b) then (generalize b = a)" $ hedgehog do
        a <- forAll genNul

        refine a & traverse_ \(b :: xs) ->
            generalize b === a

    it "if (refine a = Just b) then (assume a = b)" $ hedgehog do
        a <- forAll genNul

        refine a & traverse_ \(b :: xs) ->
            diff (assume a) sameItems b
