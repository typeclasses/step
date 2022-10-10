module Step.Toy.Spec (tests) where

import Step.Base (one, commit, nextCharMaybe)
import Step.Action.Core (AtomicMove, SureQuery)
import Step.Toy (parse)
import Step.Chunk.ListLike (NonEmptyListLike, genChunks, fold)

import qualified Step.Do as P

import Control.Monad ((>>=))
import Data.Char (Char)
import Data.Either (Either (Right))
import Data.Function (($))
import Data.Functor.Identity (Identity)
import Data.Maybe (Maybe (..))
import Data.Text (Text)
import Data.String (fromString)

import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (fromGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

type T = NonEmptyListLike Text

tests :: TestTree
tests = fromGroup $$(discover)

prop_nextCharMaybe = property do
    let parser :: SureQuery (NonEmptyListLike Text) Identity () (Maybe Char) = nextCharMaybe
    let input :: Text = "abc"
    chunkedInput :: [T] <- forAll (genChunks input)
    let (result :: Either () (Maybe Char), remainder :: [T]) = parse parser chunkedInput
    (result, remainder) === (Right (Just 'a'), chunkedInput)

prop_nextCharMaybe_and_commit = property do
    let input :: Text = "abc"
    chunkedInput :: [T] <- forAll (genChunks input)
    let parser :: AtomicMove (NonEmptyListLike Text) Identity () (Maybe Char) = nextCharMaybe P.<* commit one
    let (result :: Either () (Maybe Char), remainder :: [T]) = parse parser chunkedInput
    (result, fold remainder) === (Right (Just 'a'), "bc")
