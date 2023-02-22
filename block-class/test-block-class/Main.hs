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
        it "1" do
            let result = stateless $ span Front (pure . isUpper) str
            result `shouldBe` SpanPart (ne "ABC") (ne "defGHI")
        it "2" do
            let result = stateless $ span Back (pure . isUpper) str
            result `shouldBe` SpanPart (ne "GHI") (ne "ABCdef")

    it "SpanAll" $ hedgehog do
        end <- forAll Gen.enumBounded
        let result = stateless $ span end (pure . isLetter) str
        result === SpanAll

    it "SpanNone" $ hedgehog do
        end <- forAll Gen.enumBounded
        let result = stateless $ span end (pure . isDigit) str
        result === SpanNone

findSpec :: Spec
findSpec = describe "find" do

    describe "pivot in the middle" do
        let str = ne "abc1def2ghi"
        let digit x = pure (readMaybe [x] :: Maybe Int)
        it "1" do
            let result = stateless $ find Front digit str
            result `shouldBe` Just (Pivot (nonEmpty "abc") 1 (nonEmpty "def2ghi"))
        it "2" do
            let result = stateless $ find Back digit str
            result `shouldBe` Just (Pivot (nonEmpty "ghi") 2 (nonEmpty "abc1def"))

    it "pivot on the end" do
        let p x = pure (Just x <* guard (isLetter x))
        let result = stateless $ find Front p (ne "abc")
        result `shouldBe` Just (Pivot Nothing 'a' (nonEmpty "bc"))

    it "no pivot" $ hedgehog do
        end <- forAll Gen.enumBounded
        str <- forAll genText
        let result :: Maybe (Pivot () (NonEmpty Char))
            result = stateless $ find end (\_ -> pure Nothing) str
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
