module Main (main) where

import Essentials

import Block.Hedgehog.Spec (PredicateGenerators (..))
import Data.Char (Char)
import Data.List.NonEmpty (NonEmpty)
import Hedgehog (Gen)
import System.IO (IO)
import Test.Hspec (hspec)

import qualified Block.Hedgehog.Spec as Block
import qualified Data.Char as Char
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

main :: IO ()
main = hspec do

    Block.spec @Char @(NonEmpty Char)
        genChar
        (genNonEmpty genChar)
        pure
        genCharNonEmptyPredicate

genChar :: Gen Char
genChar = Gen.alpha

genNonEmpty :: Gen a -> Gen (NonEmpty a)
genNonEmpty g = Gen.nonEmpty (Range.linear 1 10) g

genCharNonEmptyPredicate :: PredicateGenerators Char (NonEmpty Char)
genCharNonEmptyPredicate = PredicateGenerators Char.isUpper genX genXs
  where
    genX = \case False -> Gen.lower; True -> Gen.upper
    genXs t = Gen.nonEmpty (Range.linear 1 10) (genX t)
