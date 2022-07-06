module Step.Spec.DocumentParsing where

import Step.Internal.Prelude hiding (to)

import Hedgehog

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified ListT
import qualified ListLike
import qualified Char

import Step.Test.InputChunking (genChunks)

import Test.Hspec
import Test.Hspec.Hedgehog

import Text (Text)

import Loc (loc)
import qualified SpanOrLoc

import Char (Char)

-- The modules under test
import Step.Document.Parser
import Step.Document.Prelude
import Step.Document.Error (Error (Error))
import qualified Step.Document.Do as P

spec :: SpecWith ()
spec = describe "Document parsing" do

    describe "p = char, char, char" do
        let p = P.do{ a <- char; b <- char; c <- char; P.return (a, b, c) }

        specify "(p <* end) parses \"abc\"" $ hedgehog do
            input :: [Text] <- forAll (genChunks "abc")
            let x = runIdentity $ parseOnly def (p P.<* end) (ListT.select input)
            x === Right ('a', 'b', 'c')

        specify "p parses \"abcd\"" $ hedgehog do
            input :: [Text] <- forAll (genChunks "abcd")
            let x = runIdentity $ parseOnly def p (ListT.select input)
            x === Right ('a', 'b', 'c')

        specify "(p <* end) fails on \"abcd\"" $ hedgehog do
            input :: [Text] <- forAll (genChunks "abcd")
            let x = runIdentity $ parseOnly def (p P.<* end) (ListT.select input)
            x === Left (Error [])

    describe "p = contextualize \"Digit\" (require (satisfy isDigit))" do
        let p = contextualize "Digit" (satisfy Char.isDigit)

        specify "p parses \"2\"" $ hedgehog do
            input :: [Text] <- forAll (genChunks "2")
            let x = runIdentity $ parseOnly def p (ListT.select input)
            x === Right '2'

        specify "p fails on \"a\"" $ hedgehog do
            input :: [Text] <- forAll (genChunks "a")
            let x = runIdentity $ parseOnly def p (ListT.select input)
            x === Left (Error ["Digit"])

    describe "p = repetition0 (satisfy isDigit)" do
        let p = repetition0 (satisfy Char.isDigit)

        specify "p parses 123 from 123abc" $ hedgehog do
            input :: [Text] <- forAll (genChunks "123abc")
            let x = runIdentity $ parseOnly def p (ListT.select input)
            x === Right ("123" :: [Char])

        specify "p parses nothing from abc" $ hedgehog do
            input :: [Text] <- forAll (genChunks "abc")
            let x = runIdentity $ parseOnly def p (ListT.select input)
            x === Right ([] :: [Char])

    describe "p = text \"abc\"" do
        let p = text "abc"

        specify "p parses \"abc\" and \"abcd\"" $ hedgehog do
            input :: [Text] <- forAll (Gen.element ["abc", "abcd"] >>= genChunks)
            let x = runIdentity $ parseOnly def p (ListT.select input)
            x === Right ()

        specify "p fails on any input that does not start with abc" $ hedgehog do
            input :: [Text] <- forAll (Gen.element ["", "ab", "bc"] >>= genChunks)
            let x = runIdentity $ parseOnly def p (ListT.select input)
            x === Left (Error [])

    describe "position" do

        specify "starts at 1:1" $ hedgehog do
            input :: [Text] <- forAll (Gen.element ["", "a", "bc", "abc", "abcd"] >>= genChunks)
            let x = runIdentity $ parseOnly def position (ListT.select input)
            x === Right (Loc.loc 1 1)

        specify "column is incremented by char when input contains no line breaks" $ hedgehog do
            n :: Natural <- forAll (Gen.integral (Range.linear 0 5))
            let p = P.do{ _ <- count0 n char; position }
            input :: [Text] <- forAll (genChunks (ListLike.fromList ['a' .. 'z']))
            let x = runIdentity $ parseOnly def p (ListT.select input)
            x === Right (Loc.loc 1 (fromIntegral $ n + 1))

        specify "line is incremented by char when input is line breaks" $ hedgehog do
            n :: Natural <- forAll (Gen.integral (Range.linear 0 5))
            let p = P.do{ _ <- count0 n char; position }
            input :: [Text] <- forAll (genChunks (ListLike.replicate 50 '\n'))
            let x = runIdentity $ parseOnly def p (ListT.select input)
            x === Right (Loc.loc (fromIntegral $ 1 + n) 1)

        specify "both line and column increments" $ hedgehog do
            let genInputLine = Gen.text (Range.singleton 19) Gen.alpha <&> (<> "\n")
            input :: [Text] <- forAll (genChunks =<< times 10 genInputLine)
            n :: Natural <- forAll (Gen.integral (Range.linear 0 200))
            let p = P.do{ _ <- count0 n char; position }
            let x = runIdentity $ parseOnly def p (ListT.select input)
            let (a, b) = n `quotRem` 20
            let l = Loc.loc (fromIntegral $ 1 + a) (fromIntegral $ 1 + b)
            x === Right l

    describe "withLocation" do

        specify "one-line example" $ hedgehog do
            let p = P.do{ text "abc"; x <- withLocation (text "def"); text "ghi"; P.return x }
            input :: [Text] <- forAll (genChunks "abcdefghi")
            let x = runIdentity $ parseOnly def p (ListT.select input)
            x === Right (SpanOrLoc.fromTo (loc 1 4) (loc 1 7), ())

        specify "second-line example" $ hedgehog do
            let p = P.do{ text "xyz\nabc"; x <- withLocation (text "def"); text "ghi"; P.return x }
            input :: [Text] <- forAll (genChunks "xyz\nabcdefghi")
            let x = runIdentity $ parseOnly def p (ListT.select input)
            x === Right (SpanOrLoc.fromTo (loc 2 4) (loc 2 7), ())

        specify "empty example" $ hedgehog do
            let p = P.do{ text "abc"; x <- withLocation (text ""); text "def"; P.return x }
            input :: [Text] <- forAll (genChunks "abcdef")
            let x = runIdentity $ parseOnly def p (ListT.select input)
            x === Right (SpanOrLoc.loc (loc 1 4), ())

    -- describe "while" do

    --     specify "..." $ hedgehog do
    --         let p = match
