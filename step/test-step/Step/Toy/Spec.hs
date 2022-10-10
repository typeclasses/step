module Step.Toy.Spec (tests) where

import Step.Base
import Step.Action.Core
import Step.Toy
import Step.Chunk.ListLike
import qualified Step.Do as P

import Control.Applicative
import Control.Monad ((>>=))
import Data.Char
import Data.Either
import Data.Function
import Data.Functor.Identity
import Data.Maybe
import Data.Text (Text)
import Data.String (fromString)

import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit ((@?=), testCase)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

type T = NonEmptyListLike Text

input :: Text -> Gen [NonEmptyListLike Text]
input = genChunks

tests :: TestTree
tests = fromGroup $$(discover)

prop_nextCharMaybe = property do
    i <- forAll (input "abc")
    let
        p :: SureQuery (NonEmptyListLike Text) Identity () (Maybe Char)
        p = nextCharMaybe
    parse p i === (Right (Just 'a') :: Either () (Maybe Char), ["abc"])

prop_nextCharMaybe_and_commit = property do
    i <- forAll (input "abc")
    let
        p :: AtomicMove (NonEmptyListLike Text) Identity () (Maybe Char)
        p = nextCharMaybe P.<* commit one
    parse p i === (Right (Just 'a') :: Either () (Maybe Char), ["bc"])
