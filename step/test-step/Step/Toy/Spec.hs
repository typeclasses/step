module Step.Toy.Spec (tests) where

import Step.Action
import Step.Toy
import Step.Chunk.ListLike (NonEmptyListLike, genChunks)

import qualified Step.Do as P
import qualified Step.Chunk.ListLike as LL

import Control.Monad ((>>=), Monad)
import Data.Char (Char)
import Data.Either (Either (..), either)
import Data.Eq (Eq)
import Data.Function (($), id)
import Data.Functor ((<$>))
import Data.Functor.Identity (Identity)
import Data.Maybe (Maybe (..))
import Data.Text (Text)
import Data.String (fromString)
import Text.Show (Show)
import Data.Void (Void)
import Data.Semigroup ((<>))

import qualified Data.Char as Char
import qualified Data.Text as Text

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)
import Test.Tasty.HUnit ((@?=), testCase)

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
    parseQuery peekChar noInput === Left ()

prop_peekChar_nonEmpty = property do
    x <- forAll Gen.lower
    xs <- forAll (Gen.text (Range.linear 0 3) Gen.lower)
    i <- forAll (genChunks (Text.cons x xs))

    -- peekChar, on non-empty input, should return the first character
    parseQuery peekChar i === Right x

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
    let (x, r) = parse (try takeChar) []

    -- (try takeChar), on empty input, should succeed and return Nothing
    x === (Right Nothing :: Either () (Maybe Char))

    -- there should, of course, still be no input remaining
    r === noInput

prop_takeCharMaybe_nonEmpty = property do
    x <- forAll Gen.lower
    xs <- forAll (Gen.text (Range.linear 0 3) Gen.lower)
    i <- forAll (genChunks (Text.cons x xs))
    let (e, r) = parseMaybe (try takeChar) i

    -- (try takeChar), on non-empty input, should succeed and return Just the first character
    e === Just (Just x)

    -- the remainder should be only the tail of the original input
    LL.fold r === xs

prop_takeChar_empty = withTests 1 $ property do
    let (x, r) = parse takeChar []

    -- takeChar, on empty input, should fail
    x === Left ()

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
    LL.fold r === xs

upperOrd x = if Char.isUpper x then Just (Char.ord x) else Nothing

prop_satisfyJust_empty = withTests 1 $ property do
    let (x, r) = parse (satisfyJust upperOrd) []

    -- satisfyJust, on empty input, should always fail
    x === Left ()

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
    LL.fold r === xs

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

prop_takeText_empty = property do
    xs <- LL.assume <$> forAll (Gen.text (Range.linear 1 3) Gen.alpha)
    let (x, r) = parse (takeText xs) []

    -- takeText, on empty input, should always fail
    x === Left ()

    -- there should, of course, still be no input remaining
    r === noInput

prop_takeText_notEnoughInput = property do
    a <- LL.assume <$> forAll (Gen.text (Range.linear 1 3) Gen.alpha)
    b <- LL.assume <$> forAll (Gen.text (Range.linear 1 3) Gen.alpha)
    i <- forAll (genChunks (LL.nonEmptyListLike a))
    let (r, _) = parse (takeText (a <> b)) i

    -- takeText, when the input is a proper prefix of the desired text, should fail
    r === Left ()

prop_takeText_exact = property do
    a <- LL.assume <$> forAll (Gen.text (Range.linear 1 3) Gen.alpha)
    i <- forAll (genChunks (LL.nonEmptyListLike a))
    let (x, r) = parse (takeText a) i

    -- takeText, when the input is exactly the desired text, should succeed
    x === Right ()

    -- all of the input should have been taken
    r === noInput

prop_takeText_okayAndMore = property do
    a <- LL.assume <$> forAll (Gen.text (Range.linear 1 3) Gen.alpha)
    b <- LL.assume <$> forAll (Gen.text (Range.linear 1 3) Gen.alpha)
    i <- forAll (genChunks (LL.nonEmptyListLike (a <> b)))
    let (x, r) = parse (takeText a) i

    -- takeText, when the input begins with the desired text and contains more thereafter, should succeed
    x === Right ()

    -- the remainder should consist of the input with the desired prefix stripped from it
    LL.fold r === LL.nonEmptyListLike b
