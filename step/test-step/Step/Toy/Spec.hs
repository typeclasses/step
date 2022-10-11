module Step.Toy.Spec (tests) where

import Step.Action (AtomicMove, SureQuery, Is, Any, Query, peekCharMaybe, takeCharMaybe, peekChar)
import Step.Toy (parse, parseSure, parseQuery, parseSureQuery)
import Step.Chunk.ListLike (NonEmptyListLike, genChunks, fold)

import qualified Step.Do as P

import Control.Monad ((>>=), Monad)
import Data.Char (Char)
import Data.Either (Either (..), either)
import Data.Eq (Eq)
import Data.Function (($), id)
import Data.Functor.Identity (Identity)
import Data.Maybe (Maybe (..))
import Data.Text (Text)
import Data.String (fromString)
import Text.Show (Show)
import Data.Void (Void)

import qualified Data.Text as Text

import Test.Tasty (TestTree)
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

testPureQuery :: forall m a. Monad m => Eq a => Show a =>
    Query (NonEmptyListLike Text) Identity () a -> Text -> Maybe a -> PropertyT m ()
testPureQuery parser input expectedResult = do
    chunkedInput :: [T] <- forAll (genChunks input)
    let (either (\() -> Nothing) Just -> result :: Maybe a) = parseQuery parser chunkedInput
    result === expectedResult

testPureSureQuery :: forall m a. Monad m => Eq a => Show a =>
    SureQuery (NonEmptyListLike Text) Identity Void a -> Text -> a -> PropertyT m ()
testPureSureQuery parser input expectedResult = do
    chunkedInput :: [T] <- forAll (genChunks input)
    let (result :: a, remainder :: [T]) = parseSure parser chunkedInput
    (result, remainder) === (expectedResult, chunkedInput)

testPure :: forall m a act. Monad m => Eq a => Show a => Is act Any =>
    act (NonEmptyListLike Text) Identity () a -> Text -> Maybe a -> Text -> PropertyT m ()
testPure parser input expectedResult expectedRemainder = do
    chunkedInput :: [T] <- forAll (genChunks input)
    let (either (\() -> Nothing) Just -> result :: Maybe a, remainder :: [T]) = parse parser chunkedInput
    (result, fold remainder) === (expectedResult, expectedRemainder)

prop_peekChar_empty :: Property
prop_peekChar_empty = withTests 1 $ property do
    parseQuery peekChar noInput === Left ()

prop_peekChar_nonEmpty :: Property
prop_peekChar_nonEmpty = property do
    x <- forAll Gen.lower
    xs <- forAll (Gen.text (Range.linear 0 3) Gen.lower)
    testPureQuery peekChar (Text.cons x xs) (Just x)

prop_peekCharMaybe_empty :: Property
prop_peekCharMaybe_empty = withTests 1 $ property do
    parseSureQuery peekCharMaybe noInput === Nothing

prop_peekCharMaybe_nonEmpty :: Property
prop_peekCharMaybe_nonEmpty = property do
    x <- forAll Gen.lower
    xs <- forAll (Gen.text (Range.linear 0 3) Gen.lower)
    testPureSureQuery peekCharMaybe (Text.cons x xs) (Just x)

prop_takeCharMaybe_empty :: Property
prop_takeCharMaybe_empty = withTests 1 $ property do
    parse takeCharMaybe [] === (Right Nothing :: Either () (Maybe Char), noInput)

prop_takeCharMaybe_nonEmpty :: Property
prop_takeCharMaybe_nonEmpty = property do
    x <- forAll Gen.lower
    xs <- forAll (Gen.text (Range.linear 0 3) Gen.lower)
    testPure takeCharMaybe (Text.cons x xs) (Just (Just x)) xs
