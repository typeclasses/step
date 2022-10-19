module Step.Package.InMemory.Spec (tests) where

import Step.Action
import Step.Package.General
import Step.Package.InMemory (parseMaybe, parseSureQuery, parseQueryMaybe)
import Step.Chunk.ListLike (NonEmptyListLike, genChunks)

import qualified Step.Chunk as Chunk
import qualified Step.Chunk.ListLike as LL
import qualified Step.Chunk.ListLike.Core as LL

import Data.Char (Char)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe (..))
import Data.Text (Text)
import Data.Semigroup ((<>))

import qualified Data.Char as Char
import qualified Data.Text as Text

import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (fromGroup)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

type T = NonEmptyListLike Text

noInput :: [T]
noInput = []

tests :: TestTree
tests = fromGroup $$(discover)


---  Single-character parsers  ---

prop_peekChar_empty = withTests 1 $ property do

    -- peekChar, on empty input, should fail
    parseQueryMaybe peekChar noInput === Nothing

prop_peekChar_nonEmpty = property do
    x <- forAll Gen.lower
    xs <- forAll (Gen.text (Range.linear 0 3) Gen.lower)
    i <- forAll (genChunks (Text.cons x xs))

    -- peekChar, on non-empty input, should return the first character
    parseQueryMaybe peekChar i === Just x

prop_peekCharMaybe_empty = withTests 1 $ property do

    -- (try peekChar), on empty input, should return Nothing
    parseSureQuery (try peekChar) noInput === (Nothing :: Maybe Char)

prop_peekCharMaybe_nonEmpty = property do
    x <- forAll Gen.lower
    xs <- forAll (Gen.text (Range.linear 0 3) Gen.lower)
    i <- forAll (genChunks (Text.cons x xs))

    -- (try peekChar), on non-empty input, should return Just the first character
    parseSureQuery (try peekChar) i === Just x

prop_takeCharMaybe_empty = withTests 1 $ property do
    let (x, r) = parseMaybe (try takeChar) []

    -- (try takeChar), on empty input, should succeed and return Nothing
    x === Just Nothing

    -- there should, of course, still be no input remaining
    r === noInput

prop_takeCharMaybe_nonEmpty = property do
    x <- forAll Gen.lower
    xs <- forAll (Gen.text (Range.linear 0 3) Gen.lower)
    i <- forAll (genChunks (Text.cons x xs))
    let (e, r) = parseMaybe (try takeChar) i

    -- (try takeChar), on non-empty input, should succeed and
    -- return Just the first character
    e === Just (Just x)

    -- the remainder should be only the tail of the original input
    Chunk.concatTrivialize r === xs

prop_takeChar_empty = withTests 1 $ property do
    let (x, r) = parseMaybe takeChar []

    -- takeChar, on empty input, should fail
    x === Nothing

    -- there should, of course, still be no input remaining
    r === noInput

prop_takeChar_nonEmpty = property do
    x <- forAll Gen.lower
    xs <- forAll (Gen.text (Range.linear 0 3) Gen.lower)
    i <- forAll (genChunks (Text.cons x xs))
    let (e, r) = parseMaybe takeChar i

    -- takeChar, on non-empty input, should return the first character
    e === Just x

    -- the remainder should be only the tail of the original input
    Chunk.concatTrivialize r === xs

upperOrd x = if Char.isUpper x then Just (Char.ord x) else Nothing

prop_satisfyJust_empty = withTests 1 $ property do
    let (x, r) = parseMaybe (satisfyJust upperOrd) []

    -- satisfyJust, on empty input, should always fail
    x === Nothing

    -- there should, of course, still be no input remaining
    r === noInput

prop_satisfyJust_yes = property do
    x <-forAll Gen.upper
    xs <- forAll (Gen.text (Range.linear 0 3) Gen.alpha)
    i <- forAll (genChunks (Text.cons x xs))
    let (e, r) = parseMaybe (satisfyJust upperOrd) i

    -- satisfyJust, if the first characters matches, should succeed
    e === Just (Char.ord x)

    -- the remainder after success be only the tail of the original input
    Chunk.concatTrivialize r === xs

prop_satisfyJust_no = property do
    x <- forAll Gen.lower
    xs <- forAll (Gen.text (Range.linear 0 3) Gen.alpha)
    i <- forAll (genChunks (Text.cons x xs))
    let (e, r) = parseMaybe (satisfyJust upperOrd) i

    -- satisfyJust, if the first character does not match, should fail
    e === Nothing

    -- the input should remain unchanged
    r === i


---  Particular text parsers  ---

prop_takeParticularText_empty = property do
    xs <- LL.assume <$> forAll (Gen.text (Range.linear 1 3) Gen.alpha)
    let (x, r) = parseMaybe (takeParticularText xs) []

    -- takeParticularText, on empty input, should always fail
    x === Nothing

    -- there should, of course, still be no input remaining
    r === noInput

prop_takeParticularText_notEnoughInput = property do
    a <- LL.assume <$> forAll (Gen.text (Range.linear 1 3) Gen.alpha)
    b <- LL.assume <$> forAll (Gen.text (Range.linear 1 3) Gen.alpha)
    i <- forAll (genChunks (Chunk.generalize a))
    let (r, _) = parseMaybe (takeParticularText (a <> b)) i

    -- takeParticularText, when the input is a proper prefix of
    -- the desired text, should fail
    r === Nothing

prop_takeParticularText_exact = property do
    a <- LL.assume <$> forAll (Gen.text (Range.linear 1 3) Gen.alpha)
    i <- forAll (genChunks (Chunk.generalize a))
    let (x, r) = parseMaybe (takeParticularText a) i

    -- takeParticularText, when the input is exactly the
    -- desired text, should succeed
    x === Just ()

    -- all of the input should have been taken
    r === noInput

prop_takeParticularText_okayAndMore = property do
    a <- LL.assume <$> forAll (Gen.text (Range.linear 1 3) Gen.alpha)
    b <- LL.assume <$> forAll (Gen.text (Range.linear 1 3) Gen.alpha)
    i <- forAll (genChunks (Chunk.generalize (a <> b)))
    let (x, r) = parseMaybe (takeParticularText a) i

    -- takeParticularText, when the input begins with the desired text
    -- and contains more thereafter, should succeed
    x === Just ()

    -- the remainder should consist of the input with the desired
    -- prefix stripped from it
    Chunk.concatTrivialize r === Chunk.generalize b
