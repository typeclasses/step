module Step.Toy.Spec (tests) where

import Step.Action (AtomicMove, SureQuery, Is, Any, nextCharMaybe, takeCharMaybe)
import Step.Toy (parse, parseSure)
import Step.Chunk.ListLike (NonEmptyListLike, genChunks, fold)

import qualified Step.Do as P

import Control.Monad ((>>=), Monad)
import Data.Char (Char)
import Data.Either (Either (Right))
import Data.Eq (Eq)
import Data.Function (($))
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

tests :: TestTree
tests = fromGroup $$(discover)

testPureSureQuery :: forall m a. Monad m => Eq a => Show a =>
    SureQuery (NonEmptyListLike Text) Identity Void a -> Text -> a -> PropertyT m ()
testPureSureQuery parser input expectedResult = do
    chunkedInput :: [T] <- forAll (genChunks input)
    let (result :: a, remainder :: [T]) = parseSure parser chunkedInput
    (result, remainder) === (expectedResult, chunkedInput)

testPure :: forall e m a act. Monad m => Eq e => Eq a => Show e => Show a => Is act Any =>
    act (NonEmptyListLike Text) Identity e a -> Text -> Either e a -> Text -> PropertyT m ()
testPure parser input expectedResult expectedRemainder = do
    chunkedInput :: [T] <- forAll (genChunks input)
    let (result :: Either e a, remainder :: [T]) = parse parser chunkedInput
    (result, fold remainder) === (expectedResult, expectedRemainder)

prop_nextCharMaybe :: Property
prop_nextCharMaybe = property do
    x <- forAll Gen.lower
    xs <- forAll (Gen.text (Range.linear 0 3) Gen.lower)
    testPureSureQuery nextCharMaybe (Text.cons x xs) (Just x)

prop_takeCharMaybe :: Property
prop_takeCharMaybe = property do
    x <- forAll Gen.lower
    xs <- forAll (Gen.text (Range.linear 0 3) Gen.lower)
    testPure @() takeCharMaybe (Text.cons x xs) (Right (Just x)) xs
