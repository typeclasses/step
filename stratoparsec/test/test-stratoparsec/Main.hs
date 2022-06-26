module Main (main) where

import Hedgehog

import qualified Hedgehog.Gen as Gen

import qualified ListT
import qualified Char

import qualified Stratoparsec.Document.Parser as Doc
import qualified Stratoparsec.Document.Prelude as Doc

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
