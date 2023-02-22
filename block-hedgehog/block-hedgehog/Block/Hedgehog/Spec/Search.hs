module Block.Hedgehog.Spec.Search (spec) where

import Block.Class.Search
import Essentials

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Hedgehog (Gen, forAll, (===))

import qualified Hedgehog.Gen as Gen
import qualified Block.Hedgehog.Gen.End as Gen

spec :: forall x xs.
    (Show x, Eq x) =>
    (Show xs, Eq xs) =>
    (Search x xs) =>
    Gen x -> Gen xs -> Spec
spec genX genXs = describe "Search" do
    pure ()
