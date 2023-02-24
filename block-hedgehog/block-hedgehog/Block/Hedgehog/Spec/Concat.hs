module Block.Hedgehog.Spec.Concat (spec) where

import Block.Class.Concat
import Essentials

import Hedgehog (Gen, forAll, diff)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

import qualified Block.Hedgehog.Gen.End as Gen
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

spec :: forall xs. (Show xs, Concat xs) => Gen xs -> Spec
spec genXs = describe "Concat" do

    it "(++) is associative" $ hedgehog do
        a <- forAll genXs
        b <- forAll genXs
        c <- forAll genXs
        diff ((a ++ b) ++ c) sameItems (a ++ (b ++ c))

    it "concat Front [a,b,c] = a ++ b ++ c" $ hedgehog do
        a <- forAll genXs
        b <- forAll genXs
        c <- forAll genXs
        diff (concat Front [a, b, c]) sameItems (a ++ b ++ c)

    it "concat Back [a,b,c] = c ++ b ++ a" $ hedgehog do
        a <- forAll genXs
        b <- forAll genXs
        c <- forAll genXs
        diff (concat Back [a,b,c]) sameItems (c ++ b ++ a)

    it "concat = concatRecursively" $ hedgehog do
        xss <- forAll (Gen.nonEmpty (Range.linear 1 10) genXs)
        e <- forAll Gen.end
        diff (concat e xss) sameItems (concatRecursively e xss)
