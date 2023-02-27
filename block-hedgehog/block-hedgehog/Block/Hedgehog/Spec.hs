module Block.Hedgehog.Spec (blockSpec, refinedSpec, PredicateGenerators (..)) where

import Essentials

import Block.Class (Block, Refined, ItemEquality)
import Block.Hedgehog.Spec.Search (PredicateGenerators (..))
import Hedgehog (Gen)
import Test.Hspec (Spec)

import qualified Block.Hedgehog.Spec.Block as Block
import qualified Block.Hedgehog.Spec.Concat as Concat
import qualified Block.Hedgehog.Spec.Enumerate as Enumerate
import qualified Block.Hedgehog.Spec.Index as Index
import qualified Block.Hedgehog.Spec.Singleton as Singleton
import qualified Block.Hedgehog.Spec.Positional as Positional
import qualified Block.Hedgehog.Spec.NonEmptyIso as NonEmptyIso
import qualified Block.Hedgehog.Spec.Search as Search
import qualified Block.Hedgehog.Spec.Refined as Refined

blockSpec :: forall x xs. (Eq x, ItemEquality xs, Show x, Show xs, Block x xs) =>
    Gen x -> Gen xs -> (xs -> Gen xs) -> PredicateGenerators x xs -> Spec
blockSpec genX genXs variegate genP = do
    Block.spec genXs variegate genP
    Concat.spec genXs
    Singleton.spec genX genXs variegate
    Enumerate.spec genXs variegate
    Positional.spec genXs variegate
    NonEmptyIso.spec genX genXs variegate
    Search.spec genXs variegate genP
    Index.spec genX genXs variegate

refinedSpec :: forall x nul xs.
    (Eq x, Eq nul, Eq xs, ItemEquality xs, Show x, Show nul, Show xs,
     Block x xs, Refined nul xs) =>
    Gen x -> Gen nul -> Gen xs -> (xs -> Gen xs) -> PredicateGenerators x xs -> Spec
refinedSpec genX genNul genXs variegate genP = do
    blockSpec genX genXs variegate genP
    Refined.spec genNul genXs
