module Block.Hedgehog.Spec (spec) where

import Essentials

import Hedgehog (Gen)
import Test.Hspec (Spec)
import Block.Class (Block, Item)

import qualified Block.Hedgehog.Spec.Singleton as Singleton

spec :: forall xs x.
    (Block xs, Show xs, Eq xs) =>
    (Item xs ~ x, Show x, Eq x) =>
    Gen xs -> Gen (Item xs) -> Spec
spec = Singleton.spec
