module Main (main) where

import Essentials
import Block.Class

import Control.Monad (guard)
import Data.Char (isUpper, isLetter, isDigit, Char)
import Data.Int (Int)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Prelude (error)
import System.IO (IO)
import Test.Hspec (hspec, describe, it, shouldBe, Spec)
import Text.Read (readMaybe)

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

    it "SpanAll" do
        [Back, Front] & traverse_ \end -> do
            let result = stateless $ span end (pure . isLetter) str
            result `shouldBe` SpanAll

    it "SpanNone" do
        [Back, Front] & traverse_ \end -> do
            let result = stateless $ span end (pure . isDigit) str
            result `shouldBe` SpanNone

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

    it "no pivot" do
        [Back, Front] & traverse_ \end -> do
            [ne "a", ne "ab", ne "abc"] & traverse_ \str -> do
                let result :: Maybe (Pivot () (NonEmpty Char))
                    result = stateless $ find end (\_ -> pure Nothing) str
                result `shouldBe` Nothing

biPrefixSpec :: Spec
biPrefixSpec = describe "biPrefix" do

    it "(car, carpet) -> the first is a prefix" $
        biPrefix equality (ne "car", ne "carpet")
        `shouldBe` IsPrefix First (ne "car") (ne "pet")

    it "(carpet, car) -> the second is a prefix" $
        biPrefix equality (ne "carpet", ne "car")
        `shouldBe` IsPrefix Second (ne "car") (ne "pet")

    it "First/Second property" do
        [ne "a", ne "ab", ne "abc"] & traverse_ \a -> do
            [ne "a", ne "ab", ne "abc"] & traverse_ \b -> do
                biPrefix equality (a, a <> b) `shouldBe` IsPrefix First a b
                biPrefix equality (a <> b, a) `shouldBe` IsPrefix Second a b

    it "Same" do
        [ne "a", ne "ab", ne "abc"] & traverse_ \x -> do
            biPrefix equality (x, x) `shouldBe` Same

    it "NoPrefixRelation" do
        let examples =
              [ (ne "cat", ne "frog")
              , (ne "pit", ne "pig")
              , (ne "fish", ne "fit")
              ]
        examples & traverse_ \(a, b) ->
            [(a, b), (b, a)] & traverse_ \pair ->
              biPrefix equality pair `shouldBe` NoPrefixRelation

ne :: [a] -> NonEmpty a
ne (x : xs) = x :| xs
ne [] = error "ne"
