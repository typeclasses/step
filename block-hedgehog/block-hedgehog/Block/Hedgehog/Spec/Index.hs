module Block.Hedgehog.Spec.Index (spec) where

import Block.Class.Index
import Essentials

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Hedgehog (Gen, forAll, (===), annotateShow)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Block.Class.Singleton.Types as Pop (Pop (..))
import qualified Block.Hedgehog.Gen.End as Gen

spec :: forall x xs.
    (Show x, Eq x) =>
    (Show xs, Eq xs) =>
    (Index x xs) =>
    Gen x -> Gen xs -> Spec
spec genX genXs = describe "Index" do

    it "at end 1 = Just . Pop.item . pop end" $ hedgehog do
        xs <- forAll genXs
        end <- forAll Gen.end

        at end 1 xs === (Just . Pop.item . pop end) xs
