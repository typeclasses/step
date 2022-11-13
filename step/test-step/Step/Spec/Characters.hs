module Step.Spec.Characters (tests) where

import Step.Action
import Step.Package.Characters
import Step.Package.InMemory (parseMaybe, parseSureQuery, parseQueryMaybe)
import Chunk.Text (Text1)
import Chunk.Gen (genChunks)

import qualified Chunk

import Data.Char (Char)
import Data.Function (($))
import Data.Maybe (Maybe (..))

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

prop_peekChar_empty = withTests 1 $ property do

    -- peekChar, on empty input, should fail
    parseQueryMaybe peekChar noInput () === Nothing

prop_peekChar_nonEmpty = property do
    x <- forAll Gen.lower
    xs <- forAll (Gen.text (Range.linear 0 3) Gen.lower)
    i <- forAll (genChunks @Text1 (Text.cons x xs))

    -- peekChar, on non-empty input, should return the first character
    parseQueryMaybe peekChar i () === Just x

prop_peekCharMaybe_empty = withTests 1 $ property do

    -- (try peekChar), on empty input, should return Nothing
    parseSureQuery (try peekChar) noInput () === (Nothing :: Maybe Char)

prop_peekCharMaybe_nonEmpty = property do
    x <- forAll Gen.lower
    xs <- forAll (Gen.text (Range.linear 0 3) Gen.lower)
    i <- forAll (genChunks @Text1 (Text.cons x xs))

    -- (try peekChar), on non-empty input, should return Just the first character
    parseSureQuery (try peekChar) i () === Just x

prop_takeCharMaybe_empty = withTests 1 $ property do
    let (x, r) = parseMaybe (try takeChar) [] ()

    -- (try takeChar), on empty input, should succeed and return Nothing
    x === Just Nothing

    -- there should, of course, still be no input remaining
    r === noInput

prop_takeCharMaybe_nonEmpty = property do
    x <- forAll Gen.lower
    xs <- forAll (Gen.text (Range.linear 0 3) Gen.lower)
    i <- forAll (genChunks @Text1 (Text.cons x xs))
    let (e, r) = parseMaybe (try takeChar) i ()

    -- (try takeChar), on non-empty input, should succeed and
    -- return Just the first character
    e === Just (Just x)

    -- the remainder should be only the tail of the original input
    Chunk.concatTrivialize r === xs

prop_takeChar_empty = withTests 1 $ property do
    let (x, r) = parseMaybe takeChar [] ()

    -- takeChar, on empty input, should fail
    x === Nothing

    -- there should, of course, still be no input remaining
    r === noInput

prop_takeChar_nonEmpty = property do
    x <- forAll Gen.lower
    xs <- forAll (Gen.text (Range.linear 0 3) Gen.lower)
    i <- forAll (genChunks @Text1 (Text.cons x xs))
    let (e, r) = parseMaybe takeChar i ()

    -- takeChar, on non-empty input, should return the first character
    e === Just x

    -- the remainder should be only the tail of the original input
    Chunk.concatTrivialize r === xs

upperOrd x = if Char.isUpper x then Just (Char.ord x) else Nothing

prop_satisfyJust_empty = withTests 1 $ property do
    let (x, r) = parseMaybe (satisfyJust upperOrd) [] ()

    -- satisfyJust, on empty input, should always fail
    x === Nothing

    -- there should, of course, still be no input remaining
    r === noInput

prop_satisfyJust_yes = property do
    x <-forAll Gen.upper
    xs <- forAll (Gen.text (Range.linear 0 3) Gen.alpha)
    i <- forAll (genChunks @Text1 (Text.cons x xs))
    let (e, r) = parseMaybe (satisfyJust upperOrd) i ()

    -- satisfyJust, if the first characters matches, should succeed
    e === Just (Char.ord x)

    -- the remainder after success be only the tail of the original input
    Chunk.concatTrivialize r === xs

prop_satisfyJust_no = property do
    x <- forAll Gen.lower
    xs <- forAll (Gen.text (Range.linear 0 3) Gen.alpha)
    i <- forAll (genChunks @Text1 (Text.cons x xs))
    let (e, r) = parseMaybe (satisfyJust upperOrd) i ()

    -- satisfyJust, if the first character does not match, should fail
    e === Nothing

    -- the input should remain unchanged
    r === i
