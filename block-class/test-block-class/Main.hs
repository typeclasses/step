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

    describe "Search" do

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
                it "1" $ find Front (\x -> readMaybe [x]) str `shouldBe` Just (Pivot (nonEmpty "abc") (1 :: Int) (nonEmpty "def2ghi"))
                it "2" $ find Back  (\x -> readMaybe [x]) str `shouldBe` Just (Pivot (nonEmpty "abc1def") (2 :: Int) (nonEmpty "ghi"))
            it "pivot on the end" $
                find Front (\x -> Just x <* guard (isLetter x)) (ne "abc") `shouldBe` Just (Pivot Nothing 'a' (nonEmpty "bc"))
            it "no pivot" $ hedgehog do
                end <- forAll (Gen.element [Front, Back])
                str <- forAll (Gen.element [ne "a", ne "ab", ne "abc"])
                find end (\_ -> Nothing) str === (Nothing :: Maybe (Pivot () (NonEmpty Char)))
