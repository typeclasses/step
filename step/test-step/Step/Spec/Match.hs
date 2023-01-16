module Step.Spec.Match (tests) where

import Essentials
import Hedgehog
import Step.Action
import Step.Package.FixedLength
import Step.Package.Match

import Chunk.Text (Text1)
import Chunk.Gen (genChunks)
import Step.Package.InMemory (parseMaybe)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (fromGroup)

import qualified Chunk as Chunk
import qualified Data.Text as Text
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Integer

noInput :: [Text1]
noInput = []

tests :: TestTree
tests = fromGroup $$(discover)

prop_matchSkipPositive_success = property do
    xs <- forAll (Gen.text (Range.linear 0 3) Gen.alpha)
    ys <- forAll (Gen.text (Range.linear 0 3) Gen.alpha)
    i <- forAll (genChunks @Text1 (xs <> ys))

    let (xm, r) = parseMaybe (match (castTo @Any $ skipNatural $ Integer.yolo $ Text.length xs)) i ()

    xm === Just (Chunk.refine xs, ())

    Chunk.concatMaybe r === Chunk.refine ys
