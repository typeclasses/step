module Step.Spec.FixedLength (tests) where

import Essentials
import Hedgehog
import Step.Package.FixedLength

import Chunk.Text (Text1)
import Chunk.Gen (genChunks)
import Numeric.Natural (Natural)
import Step.Package.InMemory (parseMaybe)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (fromGroup)

import qualified Chunk
import qualified Data.Text as Text
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Integer

noInput :: [Text1]
noInput = []

tests :: TestTree
tests = fromGroup $$(discover)

prop_skipPositive_success = property do
    xs <- forAll (Gen.text (Range.linear 0 3) Gen.alpha)
    ys <- forAll (Gen.text (Range.linear 0 3) Gen.alpha)
    i <- forAll (genChunks @Text1 (xs <> ys))

    let (xm, r) = parseMaybe (skipNatural $ Integer.yolo $ Text.length xs) i ()

    xm === Just ()

    Chunk.concatMaybe r === Chunk.refine ys

prop_tryTakeNatural = property do
    xs <- forAll (Gen.text (Range.linear 0 3) Gen.alpha)
    i <- forAll (genChunks @Text1 xs)
    n :: Natural <- forAll (Gen.integral (Range.linear 0 5))

    let (xm, r) = parseMaybe (tryTakeNatural n) i ()

    xm === Just (Chunk.refine (Text.take (Integer.yolo n) xs))

    Chunk.concatMaybe r === Chunk.refine (Text.drop (Integer.yolo n) xs)
