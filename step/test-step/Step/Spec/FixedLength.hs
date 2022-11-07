module Step.Spec.FixedLength (tests) where

import Step.Action
import Step.Package.FixedLength
import Step.Package.Match
import Step.Package.InMemory (parseMaybe, parseSureQuery, parseQueryMaybe)
import Step.Chunk.Text (Text1)
import Step.Chunk.Gen (genChunks)

import qualified Step.Chunk as Chunk
import qualified Step.Chunk.Text as T
import qualified Step.Chunk.Text.Core as T

import Data.Char (Char)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe (..))
import Data.Text (Text)
import Data.Semigroup ((<>))
import Data.Foldable (toList)
import Numeric.Natural (Natural)
import Prelude (fromIntegral)

import qualified Data.Char as Char
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
