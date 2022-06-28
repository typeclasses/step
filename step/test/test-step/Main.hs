module Main (main) where

import Step.Internal.Prelude

import Hedgehog

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified ListT
import qualified ListLike
import qualified Char

import qualified Step.Document.Parser as Doc
import qualified Step.Document.Prelude as Doc

import Step.Test.InputChunking (genChunks)

import Test.Hspec
import Test.Hspec.Hedgehog

import Text (Text)

import Loc (loc)
import qualified SpanOrLoc

main :: IO ()
main = hspec do
    documentParsing

documentParsing :: SpecWith ()
documentParsing = describe "Document parsing" do

    describe "p = char, char, char" do
        let c = Doc.require Doc.char
        let p = (,,) <$> c <*> c <*> c

        specify "(p <* end) parses \"abc\"" $ hedgehog do
            input :: [Text] <- forAll (genChunks "abc")
            let x = runIdentity $ Doc.parseOnly Doc.defaultErrorOptions (p <* Doc.end) (ListT.select input)
            x === Right ('a', 'b', 'c')

        specify "p parses \"abcd\"" $ hedgehog do
            input :: [Text] <- forAll (genChunks "abcd")
            let x = runIdentity $ Doc.parseOnly Doc.defaultErrorOptions p (ListT.select input)
            x === Right ('a', 'b', 'c')

        specify "(p <* end) fails on \"abcd\"" $ hedgehog do
            input :: [Text] <- forAll (genChunks "abcd")
            let x = runIdentity $ Doc.parseOnly Doc.defaultErrorOptions (p <* Doc.end) (ListT.select input)
            x === Left (Doc.Error [])

    describe "p = contextualize \"Digit\" (require (satisfy isDigit))" do
        let p = Doc.contextualize "Digit" (Doc.require (Doc.satisfy Char.isDigit))

        specify "p parses \"2\"" $ hedgehog do
            input :: [Text] <- forAll (genChunks "2")
            let x = runIdentity $ Doc.parseOnly Doc.defaultErrorOptions p (ListT.select input)
            x === Right '2'

        specify "p fails on \"a\"" $ hedgehog do
            input :: [Text] <- forAll (genChunks "a")
            let x = runIdentity $ Doc.parseOnly Doc.defaultErrorOptions p (ListT.select input)
            x === Left (Doc.Error ["Digit"])

    describe "p = many (satisfy isDigit)" do
        let p = Doc.many (Doc.satisfy Char.isDigit)

        specify "p parses 123 from 123abc" $ hedgehog do
            input :: [Text] <- forAll (genChunks "123abc")
            let x = runIdentity $ Doc.parseOnly Doc.defaultErrorOptions p (ListT.select input)
            x === Right ("123" :: [Char])

        specify "p parses nothing from abc" $ hedgehog do
            input :: [Text] <- forAll (genChunks "abc")
            let x = runIdentity $ Doc.parseOnly Doc.defaultErrorOptions p (ListT.select input)
            x === Right ([] :: [Char])

    describe "p = text \"abc\"" do
        let p = Doc.text "abc"

        specify "p parses \"abc\" and \"abcd\"" $ hedgehog do
            input :: [Text] <- forAll (Gen.element ["abc", "abcd"] >>= genChunks)
            let x = runIdentity $ Doc.parseOnly Doc.defaultErrorOptions p (ListT.select input)
            x === Right ()

        specify "p fails on any input that does not start with abc" $ hedgehog do
            input :: [Text] <- forAll (Gen.element ["", "ab", "bc"] >>= genChunks)
            let x = runIdentity $ Doc.parseOnly Doc.defaultErrorOptions p (ListT.select input)
            x === Left (Doc.Error [])

    describe "position" do

        specify "starts at 1:1" $ hedgehog do
            input :: [Text] <- forAll (Gen.element ["", "a", "bc", "abc", "abcd"] >>= genChunks)
            let x = runIdentity $ Doc.parseOnly Doc.defaultErrorOptions Doc.position (ListT.select input)
            x === Right (Loc.loc 1 1)

        specify "column is incremented by char when input contains no line breaks" $ hedgehog do
            n :: Natural <- forAll (Gen.integral (Range.linear 0 5))
            let p = appEndo (times n (Endo (Doc.require Doc.char *>))) Doc.position
            input :: [Text] <- forAll (genChunks (ListLike.fromList ['a' .. 'z']))
            let x = runIdentity $ Doc.parseOnly Doc.defaultErrorOptions p (ListT.select input)
            x === Right (Loc.loc 1 (fromIntegral $ n + 1))

        specify "line is incremented by char when input is line breaks" $ hedgehog do
            n :: Natural <- forAll (Gen.integral (Range.linear 0 5))
            let p = appEndo (times n (Endo (Doc.require Doc.char *>))) Doc.position
            input :: [Text] <- forAll (genChunks (ListLike.replicate 50 '\n'))
            let x = runIdentity $ Doc.parseOnly Doc.defaultErrorOptions p (ListT.select input)
            x === Right (Loc.loc (fromIntegral $ 1 + n) 1)

    describe "withLocation" do

        specify "one-line example" $ hedgehog do
            let p = Doc.text "abc" *> Doc.withLocation (Doc.text "def") <* Doc.text "ghi"
            input :: [Text] <- forAll (genChunks "abcdefghi")
            let x = runIdentity $ Doc.parseOnly Doc.defaultErrorOptions p (ListT.select input)
            x === Right (SpanOrLoc.fromTo (loc 1 4) (loc 1 7), ())

        specify "second-line example" $ hedgehog do
            let p = Doc.text "xyz\nabc" *> Doc.withLocation (Doc.text "def") <* Doc.text "ghi"
            input :: [Text] <- forAll (genChunks "xyz\nabcdefghi")
            let x = runIdentity $ Doc.parseOnly Doc.defaultErrorOptions p (ListT.select input)
            x === Right (SpanOrLoc.fromTo (loc 2 4) (loc 2 7), ())

        specify "empty example" $ hedgehog do
            let p = Doc.text "abc" *> Doc.withLocation (Doc.text "") <* Doc.text "def"
            input :: [Text] <- forAll (genChunks "abcdef")
            let x = runIdentity $ Doc.parseOnly Doc.defaultErrorOptions p (ListT.select input)
            x === Right (SpanOrLoc.loc (loc 1 4), ())
