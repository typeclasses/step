module Step.Toy.Spec (tests) where

import Step.Base
import Step.Toy
import Step.Chunk.ListLike
import qualified Step.Do as P

import Control.Applicative
import Data.Char
import Data.Either
import Data.Function
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

tests :: TestTree
tests = testGroup "Toy"
  [ testCase "nextCharMaybe" $
      parse nextCharMaybe ["abc" :: T] @?=
          (Right (Just 'a') :: Either () (Maybe Char), ["abc"])
  , testCase "nextCharMaybe and commit" $
      parse (nextCharMaybe P.<* commit one) ["abc" :: T] @?=
          (Right (Just 'a') :: Either () (Maybe Char), ["bc"])
  ]
