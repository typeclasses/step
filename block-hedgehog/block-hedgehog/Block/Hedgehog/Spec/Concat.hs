module Block.Hedgehog.Spec.Concat (spec) where

import Block.Class.Concat
import Essentials

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Hedgehog (Gen, forAll, (===), annotateShow)
import Block.Hedgehog.Gen.Positive (positive)
import Prelude ((+))

import qualified Data.Foldable as Foldable
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Block.Hedgehog.Gen.End as Gen

spec :: forall xs.
    (Show xs, Eq xs) =>
    (Concat xs) =>
    Gen xs -> Spec
spec genXs = describe "Concat" do

    it "(++) is associative" $ hedgehog do
        a <- forAll genXs
        b <- forAll genXs
        c <- forAll genXs

        (a ++ b) ++ c === a ++ (b ++ c)

    it "concat Front [a,b,c] = a ++ b ++ c" $ hedgehog do
        a <- forAll genXs
        b <- forAll genXs
        c <- forAll genXs

        concat Front [a, b, c] === a ++ b ++ c

    it "concat Back [a,b,c] = c ++ b ++ a" $ hedgehog do
        a <- forAll genXs
        b <- forAll genXs
        c <- forAll genXs

        concat Back [a,b,c] === c ++ b ++ a

    it "concat = concatRecursively" $ hedgehog do
        xss <- forAll (Gen.nonEmpty (Range.linear 1 10) genXs)
        e <- forAll Gen.end

        concat e xss === concatRecursively e xss
