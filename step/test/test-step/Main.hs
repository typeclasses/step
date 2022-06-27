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

import qualified Loc

main :: IO ()
main = hspec do
    documentParsing

documentParsing :: SpecWith ()
documentParsing = describe "Document parsing" do

    describe "p = char, char, char" do
        let p = (,,) <$> Doc.char <*> Doc.char <*> Doc.char

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

    describe "p = contextualize \"Digit\" (satisfy isDigit)" do
        let p = Doc.contextualize "Digit" (Doc.satisfy Char.isDigit)

        specify "p parses \"2\"" $ hedgehog do
            let input :: [Text] = ["2"]
            let x = runIdentity $ Doc.parseOnly Doc.defaultErrorOptions p (ListT.select input)
            x === Right '2'

        specify "p fails on \"a\"" $ hedgehog do
            let input :: [Text] = ["a"]
            let x = runIdentity $ Doc.parseOnly Doc.defaultErrorOptions p (ListT.select input)
            x === Left (Doc.Error ["Digit"])

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
            let p = appEndo (times n (Endo (Doc.char *>))) Doc.position
            input :: [Text] <- forAll (genChunks (ListLike.fromList ['a' .. 'z']))
            let x = runIdentity $ Doc.parseOnly Doc.defaultErrorOptions p (ListT.select input)
            x === Right (Loc.loc 1 (fromIntegral $ n + 1))

        specify "line is incremented by char when input is line breaks" $ hedgehog do
            n :: Natural <- forAll (Gen.integral (Range.linear 0 5))
            let p = appEndo (times n (Endo (Doc.char *>))) Doc.position
            input :: [Text] <- forAll (genChunks (ListLike.replicate 50 '\n'))
            let x = runIdentity $ Doc.parseOnly Doc.defaultErrorOptions p (ListT.select input)
            x === Right (Loc.loc (fromIntegral $ 1 + n) 1)
