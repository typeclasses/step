module Main (main) where

import Essentials
import Block.Class

import Control.Monad (guard)
import Data.Char (isUpper, isLetter, isDigit, Char)
import Data.Int (Int)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Hedgehog (forAll, (===), Gen)
import Prelude (error)
import System.IO (IO)
import Test.Hspec (hspec, describe, it, shouldBe, Spec)
import Test.Hspec.Hedgehog (hedgehog)
import Text.Read (readMaybe)

import qualified Hedgehog.Gen as Gen

main :: IO ()
main = hspec do
    spanSpec
    findSpec
    biPrefixSpec

spanSpec :: Spec
spanSpec = describe "span" do
    let str = ne "ABCdefGHI"

    describe "SpanPart" do
        it "1" $ span Front isUpper str `shouldBe`
            SpanPart (ne "ABC") (ne "defGHI")
        it "2" $ span Back isUpper str `shouldBe`
            SpanPart (ne "GHI") (ne "ABCdef")

    it "SpanAll" $ hedgehog do
        end <- forAll Gen.enumBounded
        span end isLetter str === SpanAll

    it "SpanNone" $ hedgehog do
        end <- forAll Gen.enumBounded
        span end isDigit str === SpanNone

findSpec :: Spec
findSpec = describe "find" do

    describe "pivot in the middle" do
        let str = ne "abc1def2ghi"
        let digit x = readMaybe [x] :: Maybe Int
        it "1" $ find Front digit str `shouldBe`
            Just (Pivot (nonEmpty "abc") 1 (nonEmpty "def2ghi"))
        it "2" $ find Back digit str `shouldBe`
            Just (Pivot (nonEmpty "ghi") 2 (nonEmpty "abc1def"))

    it "pivot on the end" $
        find Front (\x -> Just x <* guard (isLetter x)) (ne "abc")
        `shouldBe` Just (Pivot Nothing 'a' (nonEmpty "bc"))

    it "no pivot" $ hedgehog do
        end <- forAll Gen.enumBounded
        str <- forAll genText
        let result :: Maybe (Pivot () (NonEmpty Char))
            result = find end (\_ -> Nothing) str
        result === Nothing

biPrefixSpec :: Spec
biPrefixSpec = describe "biPrefix" do

    it "(car, carpet) -> the first is a prefix" $
        biPrefix equality (ne "car", ne "carpet")
        `shouldBe` IsPrefix First (ne "car") (ne "pet")

    it "(carpet, car) -> the second is a prefix" $
        biPrefix equality (ne "carpet", ne "car")
        `shouldBe` IsPrefix Second (ne "car") (ne "pet")

    it "First/Second property" $ hedgehog do
        a <- forAll genText
        b <- forAll genText
        biPrefix equality (a, a <> b) === IsPrefix First a b
        biPrefix equality (a <> b, a) === IsPrefix Second a b

    it "Same" $ hedgehog do
        x <- forAll genText
        biPrefix equality (x, x) === Same

    it "NoPrefixRelation" $ hedgehog do
        (a, b) <- forAll $ Gen.element
          [ (ne "cat", ne "frog")
          , (ne "pit", ne "pig")
          , (ne "fish", ne "fit")
          ]
        pair <- forAll $ Gen.element [(a, b), (b, a)]
        biPrefix equality pair === NoPrefixRelation

ne :: [a] -> NonEmpty a
ne (x : xs) = x :| xs
ne [] = error "ne"

genText :: Gen (NonEmpty Char)
genText = Gen.element [ne "a", ne "ab", ne "abc"]
