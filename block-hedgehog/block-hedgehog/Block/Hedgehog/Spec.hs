module Block.Hedgehog.Spec (spec) where

import Essentials
import Block.Class

import Hedgehog (Gen)
import Test.Hspec (Spec)

import qualified Block.Hedgehog.Spec.Singleton as Singleton

spec :: forall x xs.
    (Show x, Eq x) =>
    (Show xs, Eq xs) =>
    (Semigroup xs, Singleton x xs) =>
    Gen x -> Gen xs -> Spec
spec = Singleton.spec
