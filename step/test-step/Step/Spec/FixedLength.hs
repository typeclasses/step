module Step.Spec.FixedLength (tests) where

import Step.Package.FixedLength
import Step.Package.InMemory (parseMaybe)
import Chunk.Text (Text1)
import Chunk.Gen (genChunks)

import qualified Chunk

import Data.Function (($))
import Data.Maybe (Maybe (..))
import Data.Semigroup ((<>))
import Numeric.Natural (Natural)
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

prop_skipPositive_success = property do
    xs <- forAll (Gen.text (Range.linear 0 3) Gen.alpha)
    ys <- forAll (Gen.text (Range.linear 0 3) Gen.alpha)
    i <- forAll (genChunks @Text1 (xs <> ys))

    let (xm, r) = parseMaybe (skipNatural (fromIntegral $ Text.length xs)) i ()

    xm === Just ()

    Chunk.concatMaybe r === Chunk.refine ys

prop_tryTakeNatural = property do
    xs <- forAll (Gen.text (Range.linear 0 3) Gen.alpha)
    i <- forAll (genChunks @Text1 xs)
    n :: Natural <- forAll (Gen.integral (Range.linear 0 5))

    let (xm, r) = parseMaybe (tryTakeNatural n) i ()

    xm === Just (Chunk.refine (Text.take (fromIntegral n) xs))

    Chunk.concatMaybe r === Chunk.refine (Text.drop (fromIntegral n) xs)
