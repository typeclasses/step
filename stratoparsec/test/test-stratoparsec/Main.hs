module Main (main) where

import Hedgehog

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified ListT
import qualified Char
import qualified Text

import qualified Stratoparsec.Document.Parser as Doc
import qualified Stratoparsec.Document.Prelude as Doc
import qualified Stratoparsec.Document.Position as Doc

import Stratoparsec.Test.InputChunking (genChunks)

import Test.Hspec
import Test.Hspec.Hedgehog

main :: IO ()
main = hspec do
    documentParsing

documentParsing :: SpecWith ()
documentParsing = describe "Document parsing" do

    describe "p = char, char, char" do
        let p = (,,) <$> Doc.char <*> Doc.char <*> Doc.char
        specify "(p <* end) parses \"abc\"" $ hedgehog do
            input <- forAll (genChunks "abc")
            x <- Doc.parseOnly Doc.defaultErrorOptions (p <* Doc.end) (ListT.select input)
            x === Right ('a', 'b', 'c')
        specify "p parses \"abcd\"" $ hedgehog do
            input <- forAll (genChunks "abcd")
            x <- Doc.parseOnly Doc.defaultErrorOptions p (ListT.select input)
            x === Right ('a', 'b', 'c')
        specify "(p <* end) fails on \"abcd\"" $ hedgehog do
            input <- forAll (genChunks "abcd")
            x <- Doc.parseOnly Doc.defaultErrorOptions (p <* Doc.end) (ListT.select input)
            x === Left (Doc.Error [])

    describe "p = contextualize \"Digit\" (satisfy isDigit)" do
        let p = Doc.contextualize "Digit" (Doc.satisfy Char.isDigit)
        specify "p parses \"2\"" $ hedgehog do
            x <- Doc.parseOnly Doc.defaultErrorOptions p (ListT.select ["2"])
            x === Right '2'
        specify "p fails on \"a\"" $ hedgehog do
            x <- Doc.parseOnly Doc.defaultErrorOptions p (ListT.select ["a"])
            x === Left (Doc.Error ["Digit"])

    describe "p = text \"abc\"" do
        let p = Doc.text "abc"
        specify "p parses \"abc\" and \"abcd\"" $ hedgehog do
            input <- forAll (Gen.element ["abc", "abcd"] >>= genChunks)
            x <- Doc.parseOnly Doc.defaultErrorOptions p (ListT.select input)
            x === Right ()
        specify "p fails on any input that does not start with abc" $ hedgehog do
            input <- forAll (Gen.element ["", "ab", "bc"] >>= genChunks)
            x <- Doc.parseOnly Doc.defaultErrorOptions p (ListT.select input)
            x === Left (Doc.Error [])

    describe "position" do
        specify "starts at 1:1" $ hedgehog do
            input <- forAll (Gen.element ["", "a", "bc", "abc", "abcd"] >>= genChunks)
            x <- Doc.parseOnly Doc.defaultErrorOptions Doc.position (ListT.select input)
            x === Right (Doc.Position 1 1)
        specify "column is incremented by char when input contains no line breaks" $ hedgehog do
            n :: Natural <- forAll (Gen.integral (Range.linear 0 5))
            let p = appEndo (stimes n (Endo (Doc.char *>))) Doc.position
            input <- forAll (genChunks (Text.pack ['a' .. 'z']))
            x <- Doc.parseOnly Doc.defaultErrorOptions p (ListT.select input)
            x === Right (Doc.Position 1 (Doc.ColumnNumber (n + 1)))
        specify "line is incremented by char when input is line breaks" $ hedgehog do
            n :: Natural <- forAll (Gen.integral (Range.linear 0 5))
            let p = appEndo (stimes n (Endo (Doc.char *>))) Doc.position
            input <- forAll (genChunks (Text.replicate 50 "\n"))
            x <- Doc.parseOnly Doc.defaultErrorOptions p (ListT.select input)
            x === Right (Doc.Position (Doc.LineNumber (1 + n)) 1)
