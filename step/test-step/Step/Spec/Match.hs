module Step.Spec.Match (tests) where

import Step.Action
import Step.Package.FixedLength
import Step.Package.Match
import Step.Package.InMemory (parseMaybe)
import Step.Chunk.Text (Text1)
import Step.Chunk.Gen (genChunks)

import qualified Step.Chunk as Chunk

import Data.Function (($))
import Data.Maybe (Maybe (..))
import Data.Semigroup ((<>))
import Prelude (fromIntegral)

import qualified Data.Text as Text

import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (fromGroup)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

noInput :: [Text1]
noInput = []

tests :: TestTree
tests = fromGroup $$(discover)

prop_matchSkipPositive_success = property do
    xs <- forAll (Gen.text (Range.linear 0 3) Gen.alpha)
    ys <- forAll (Gen.text (Range.linear 0 3) Gen.alpha)
    i <- forAll (genChunks @Text1 (xs <> ys))

    let (xm, r) = parseMaybe (match (castTo @Any $ skipNatural (fromIntegral $ Text.length xs))) i ()

    xm === Just (Chunk.refine xs, ())

    Chunk.concatMaybe r === Chunk.refine ys
