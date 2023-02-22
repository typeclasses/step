module Block.Hedgehog.Spec.Search (spec, PredicateGenerators (..)) where

import Block.Class.Search
import Essentials

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Hedgehog (Gen, forAll, (===))

import qualified Hedgehog.Gen as Gen
import qualified Block.Hedgehog.Gen.End as Gen

data PredicateGenerators x xs =
  PredicateGenerators
    (x -> Bool) -- ^ A predicate
    (Bool -> Gen x) -- ^ Item generators for items that do/don't match the predicate
    (Bool -> Gen xs) -- ^ Block generators for blocks whose items all do/don't match the predicate

spec :: forall x xs.
    (Show x, Eq x) =>
    (Show xs, Eq xs) =>
    (Search x xs) =>
    Gen x -> Gen xs
    -> PredicateGenerators x xs
    -> Spec
spec genX genXs (PredicateGenerators p genX' genXs') = describe "Search" do
    pure ()
