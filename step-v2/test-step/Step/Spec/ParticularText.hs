module Step.Spec.ParticularText (tests) where

import Essentials
import Step.Package.ParticularText
import Hedgehog

import Step.Package.InMemory (parseMaybe)
import Block.Text (Text1)
import Block.Gen (genBlocks)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (fromGroup)

import qualified Block.Class as Chunk
import qualified Block.Text as T
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

noInput :: [Text1]
noInput = []

tests :: TestTree
tests = fromGroup $$(discover)

prop_takeParticularText_empty = property do
    xs <- T.assume <$> forAll (Gen.text (Range.linear 1 3) Gen.alpha)
    let (x, r) = parseMaybe (takeParticularText xs) [] ()

    -- takeParticularText, on empty input, should always fail
    x === Nothing

    -- there should, of course, still be no input remaining
    r === noInput

prop_takeParticularText_notEnoughInput = property do
    a <- T.assume <$> forAll (Gen.text (Range.linear 1 3) Gen.alpha)
    b <- T.assume <$> forAll (Gen.text (Range.linear 1 3) Gen.alpha)
    i <- forAll (genBlocks (Chunk.generalize a))
    let (r, _) = parseMaybe (takeParticularText (a <> b)) i ()

    -- takeParticularText, when the input is a proper prefix of
    -- the desired text, should fail
    r === Nothing

prop_takeParticularText_exact = property do
    a <- T.assume <$> forAll (Gen.text (Range.linear 1 3) Gen.alpha)
    i <- forAll (genBlocks (Chunk.generalize a))
    let (x, r) = parseMaybe (takeParticularText a) i ()

    -- takeParticularText, when the input is exactly the
    -- desired text, should succeed
    x === Just ()

    -- all of the input should have been taken
    r === noInput

prop_takeParticularText_okayAndMore = property do
    a <- T.assume <$> forAll (Gen.text (Range.linear 1 3) Gen.alpha)
    b <- T.assume <$> forAll (Gen.text (Range.linear 1 3) Gen.alpha)
    i <- forAll (genBlocks (Chunk.generalize (a <> b)))
    let (x, r) = parseMaybe (takeParticularText a) i ()

    -- takeParticularText, when the input begins with the desired text
    -- and contains more thereafter, should succeed
    x === Just ()

    -- the remainder should consist of the input with the desired
    -- prefix stripped from it
    Chunk.concatTrivialize r === Chunk.generalize b
