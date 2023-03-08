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

import qualified Fold.ShortcutNonempty as Fold

main :: IO ()
main = hspec do
    spanSpec
    findSpec
    biPrefixSpec
    foldSpec
    atSpec

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
            result `shouldBe` Just Pivot
                { pivot = 1
                , split1 = (ne "abc1", nonEmpty "def2ghi")
                , split2 = (nonEmpty "abc", ne "1def2ghi")
                }
        it "2" do
            let result = stateless $ find Back digit str
            result `shouldBe` Just Pivot
                { pivot = 2
                , split1 = (ne "2ghi", nonEmpty "abc1def")
                , split2 = (nonEmpty "ghi", ne "abc1def2")
                }

    it "pivot on the end" do
        let p x = pure (Just x <* guard (isLetter x))
        let result = stateless $ find Front p (ne "abc")
        result `shouldBe` Just Pivot
            { pivot = 'a'
            , split1 = (ne "a", nonEmpty "bc")
            , split2 = (Nothing, ne "abc")
            }

    it "no pivot" do
        [Back, Front] & traverse_ \end -> do
            [ne "a", ne "ab", ne "abc"] & traverse_ \str -> do
                let result :: Maybe (Pivot () (NonEmpty Char))
                    result = stateless $ find end (\_ -> pure Nothing) str
                result `shouldBe` Nothing

biPrefixSpec :: Spec
biPrefixSpec = describe "biPrefix" do

    it "(car, carpet) -> the first is a prefix" $
        biPrefix equality Front (ne "car", ne "carpet")
        `shouldBe` IsPrefix First (ne "car") (ne "pet")

    it "(carpet, car) -> the second is a prefix" $
        biPrefix equality Front (ne "carpet", ne "car")
        `shouldBe` IsPrefix Second (ne "car") (ne "pet")

    it "First/Second property" do
        [ne "a", ne "ab", ne "abc"] & traverse_ \a -> do
            [ne "a", ne "ab", ne "abc"] & traverse_ \b -> do
                biPrefix equality Front (a, a <> b) `shouldBe` IsPrefix First a b
                biPrefix equality Front (a <> b, a) `shouldBe` IsPrefix Second a b

    it "BothPrefix" do
        [ne "a", ne "ab", ne "abc"] & traverse_ \x -> do
            biPrefix equality Front (x, x) `shouldBe` BothPrefix

    it "NoPrefixRelation" do
        let examples =
              [ (ne "cat", ne "frog")
              , (ne "pit", ne "pig")
              , (ne "fish", ne "fit")
              ]
        examples & traverse_ \(a, b) ->
            [(a, b), (b, a)] & traverse_ \pair ->
              biPrefix equality Front pair `shouldBe` NoPrefixRelation

foldSpec :: Spec
foldSpec = describe "foldItems" do
    it "Front" $
        foldItems Front Fold.list (ne "abc") `shouldBe` (ne "abc")
    it "Back" $
        foldItems Back Fold.list (ne "abc") `shouldBe` (ne "cba")

atSpec :: Spec
atSpec = describe "at" do
    it "Front" do
        at Front 1 (ne "abc") `shouldBe` Just 'a'
        at Front 2 (ne "abc") `shouldBe` Just 'b'
        at Front 3 (ne "abc") `shouldBe` Just 'c'
        at Front 4 (ne "abc") `shouldBe` Nothing
    it "Back" do
        at Back 1 (ne "abc") `shouldBe` Just 'c'
        at Back 2 (ne "abc") `shouldBe` Just 'b'
        at Back 3 (ne "abc") `shouldBe` Just 'a'
        at Back 4 (ne "abc") `shouldBe` Nothing

ne :: [a] -> NonEmpty a
ne (x : xs) = x :| xs
ne [] = error "ne"
