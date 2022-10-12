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
import Test.Tasty.Hedgehog (testPropertyNamed)
import Test.Tasty.HUnit ((@?=), testCase)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

type T = NonEmptyListLike Text

noInput :: [T]
noInput = []

tests :: TestTree
tests =
  testGroup "Toy"
    [ singleCharacterTests
    , particularTextTests
    ]

singleCharacterTests = testGroup "Single characters"
  [ testGroup "peekChar"
    [ testPropertyNamed "empty" "prop_peekChar_empty" prop_peekChar_empty
    , testPropertyNamed "non-empty" "prop_peekChar_nonEmpty" prop_peekChar_nonEmpty
    ]
  , testGroup "peekCharMaybe"
    [ testPropertyNamed "empty" "prop_peekCharMaybe_empty" prop_peekCharMaybe_empty
    , testPropertyNamed "non-empty" "prop_peekCharMaybe_nonEmpty" prop_peekCharMaybe_nonEmpty
    ]
  , testGroup "takeCharMaybe"
    [ testPropertyNamed "empty" "prop_takeCharMaybe_empty" prop_takeCharMaybe_empty
    , testPropertyNamed "non-empty" "prop_takeCharMaybe_nonEmpty" prop_takeCharMaybe_nonEmpty
    ]
  , testGroup "takeChar"
    [ testPropertyNamed "empty" "prop_takeChar_empty" prop_takeChar_empty
    , testPropertyNamed "non-empty" "prop_takeChar_nonEmpty" prop_takeChar_nonEmpty
    ]
  , testGroup "satisfyJust"
    [ testPropertyNamed "empty" "prop_satisfyJust_empty" prop_satisfyJust_empty
    , testPropertyNamed "yes" "prop_satisfyJust_yes" prop_satisfyJust_yes
    , testPropertyNamed "no" "prop_satisfyJust_no" prop_satisfyJust_no
    ]
  ]

particularTextTests = testGroup "Particular text"
  [ testGroup "takeText"
      [ testPropertyNamed "empty" "prop_takeText_empty" prop_takeText_empty
      , testPropertyNamed "not enough input" "prop_takeText_notEnoughInput" prop_takeText_notEnoughInput
      , testPropertyNamed "exact" "prop_takeText_exact" prop_takeText_exact
      , testPropertyNamed "okay and more" "prop_takeText_okayAndMore" prop_takeText_okayAndMore
      ]
  ]

prop_peekChar_empty = withTests 1 $ property do
    parseQuery peekChar noInput === Left ()

prop_peekChar_nonEmpty = property do
    x <- forAll Gen.lower
    xs <- forAll (Gen.text (Range.linear 0 3) Gen.lower)
    i <- forAll (genChunks (Text.cons x xs))
    parseQuery peekChar i === Right x

prop_peekCharMaybe_empty = withTests 1 $ property do
    parseSureQuery peekCharMaybe noInput === Nothing

prop_peekCharMaybe_nonEmpty = property do
    x <- forAll Gen.lower
    xs <- forAll (Gen.text (Range.linear 0 3) Gen.lower)
    i <- forAll (genChunks (Text.cons x xs))
    parseSureQuery peekCharMaybe i === Just x

prop_takeCharMaybe_empty = withTests 1 $ property do
    parse takeCharMaybe [] === (Right Nothing :: Either () (Maybe Char), noInput)

prop_takeCharMaybe_nonEmpty = property do
    x <- forAll Gen.lower
    xs <- forAll (Gen.text (Range.linear 0 3) Gen.lower)
    i <- forAll (genChunks (Text.cons x xs))
    let (e, r) = parseMaybe takeCharMaybe i
    e === Just (Just x)
    LL.fold r === xs

prop_takeChar_empty = withTests 1 $ property do
    parse takeChar [] === (Left (), noInput)

prop_takeChar_nonEmpty = property do
    x <- forAll Gen.lower
    xs <- forAll (Gen.text (Range.linear 0 3) Gen.lower)
    i <- forAll (genChunks (Text.cons x xs))
    let (e, r) = parseMaybe takeChar i
    e === Just x
    LL.fold r === xs

upperOrd x = if Char.isUpper x then Just (Char.ord x) else Nothing

prop_satisfyJust_empty = withTests 1 $ property do
    parse (satisfyJust upperOrd) [] === (Left (), noInput)

prop_satisfyJust_yes = property do
    x <-forAll Gen.upper
    xs <- forAll (Gen.text (Range.linear 0 3) Gen.alpha)
    i <- forAll (genChunks (Text.cons x xs))
    let (e, r) = parseMaybe (satisfyJust upperOrd) i
    e === Just (Char.ord x)
    LL.fold r === xs

prop_satisfyJust_no = property do
    x <- forAll Gen.lower
    xs <- forAll (Gen.text (Range.linear 0 3) Gen.alpha)
    i <- forAll (genChunks (Text.cons x xs))
    let (e, r) = parseMaybe (satisfyJust upperOrd) i
    e === Nothing
    r === i

prop_takeText_empty = property do
    xs <- LL.assume <$> forAll (Gen.text (Range.linear 1 3) Gen.alpha)
    parse (takeText xs) [] === (Left (), [])

prop_takeText_notEnoughInput = property do
    a <- LL.assume <$> forAll (Gen.text (Range.linear 1 3) Gen.alpha)
    b <- LL.assume <$> forAll (Gen.text (Range.linear 1 3) Gen.alpha)
    i <- forAll (genChunks (LL.nonEmptyListLike a))
    let (r, _) = parse (takeText (a <> b)) i
    r === Left ()

prop_takeText_exact = property do
    a <- LL.assume <$> forAll (Gen.text (Range.linear 1 3) Gen.alpha)
    i <- forAll (genChunks (LL.nonEmptyListLike a))
    let (r, _) = parse (takeText a) i
    r === Right ()

prop_takeText_okayAndMore = property do
    a <- LL.assume <$> forAll (Gen.text (Range.linear 1 3) Gen.alpha)
    b <- LL.assume <$> forAll (Gen.text (Range.linear 1 3) Gen.alpha)
    i <- forAll (genChunks (LL.nonEmptyListLike (a <> b)))
    let (x, r) = parse (takeText a) i
    x === Right ()
    LL.fold r === LL.nonEmptyListLike b
