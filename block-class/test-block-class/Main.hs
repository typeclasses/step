module Main (main) where

import Essentials
import Block.Class

import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Test.Hspec (hspec, describe, it, shouldBe)
import Test.Hspec.Hedgehog (hedgehog)
import System.IO (IO)
import Data.Char (isUpper, isLetter, isDigit, Char)
import Text.Read (readMaybe)
import Control.Monad (guard)
import Data.Int (Int)
import Hedgehog (forAll, (===))
import Prelude (error)

import qualified Hedgehog.Gen as Gen

ne :: [a] -> NonEmpty a
ne (x : xs) = x :| xs
ne [] = error "ne"

main :: IO ()
main = hspec do

    describe "span" do
        let str = ne "ABCdefGHI"
        describe "part" do
            it "1" $ span Front isUpper  str `shouldBe` SpanPart (ne "ABC") (ne "defGHI")
            it "2" $ span Back  isUpper  str `shouldBe` SpanPart (ne "GHI") (ne "ABCdef")
        it "all" $ span Back  isLetter str `shouldBe` SpanAll
        it "none" $ span Back  isDigit  str `shouldBe` SpanNone

    describe "find" do
        describe "pivot in the middle" do
            let str = ne "abc1def2ghi"
            let digit x = readMaybe [x] :: Maybe Int
            it "1" $ find Front digit str `shouldBe` Just (Pivot (nonEmpty "abc") (1 :: Int) (nonEmpty "def2ghi"))
            it "2" $ find Back  digit str `shouldBe` Just (Pivot (nonEmpty "ghi") (2 :: Int) (nonEmpty "abc1def"))
        it "pivot on the end" $
            find Front (\x -> Just x <* guard (isLetter x)) (ne "abc") `shouldBe` Just (Pivot Nothing 'a' (nonEmpty "bc"))
        it "no pivot" $ hedgehog do
            end <- forAll (Gen.element [Front, Back])
            str <- forAll (Gen.element [ne "a", ne "ab", ne "abc"])
            find end (\_ -> Nothing) str === (Nothing :: Maybe (Pivot () (NonEmpty Char)))

    describe "biPrefix" do
        it "First" $ biPrefix equality (ne "ab", ne "abcde") `shouldBe` IsPrefix First (ne "ab") (ne "abcde")
        it "Second" $ biPrefix equality (ne "abcde", ne "ab") `shouldBe` IsPrefix Second (ne "ab") (ne "abcde")
        it "Same" $ biPrefix equality (ne "abcd", ne "abcd") `shouldBe` Same
        describe "NoPrefixRelation" do
            it "1" $ biPrefix equality (ne "abc", ne "xabc") `shouldBe` NoPrefixRelation
            it "2" $ biPrefix equality (ne "abc", ne "xyz") `shouldBe` NoPrefixRelation
