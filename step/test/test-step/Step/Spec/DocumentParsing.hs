{-# language FlexibleContexts, QualifiedDo, OverloadedStrings #-}

module Step.Spec.DocumentParsing where

import Step.Internal.Prelude

import Test.Hspec
import Test.Hspec.Hedgehog

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Step.Test.InputChunking (genChunks)

import qualified ListLike

import Loc (loc)
import qualified Loc
import qualified SpanOrLoc

import qualified Char
import Char (Char)

import Text (Text)

import qualified Step.Input.Stream as Stream
import Step.Nontrivial.Constructor (Nontrivial (NontrivialUnsafe))

-- The module under test
import qualified Step.Document as P

spec :: SpecWith ()
spec = describe "Document parsing" do

    describe "p = char, char, char" do
        let p = P.do{ a <- P.satisfyJust Just; b <- P.satisfyJust Just; c <- P.satisfyJust Just; P.return (a, b, c) }
        let end = P.atEnd P.>>= \case{ True -> P.cast @P.Query (P.return ()); False -> P.cast @P.Query P.failure }

        specify "(p <* end) parses \"abc\"" $ hedgehog do
            input :: [Text] <- forAll (genChunks "abc")
            let x = input & evalStateT (P.parseOnly def (p P.<* end) Stream.list) & runIdentity
            x === Right ('a', 'b', 'c')

        specify "p parses \"abcd\"" $ hedgehog do
            input :: [Text] <- forAll (genChunks "abcd")
            let x = input & evalStateT (P.parseOnly def p Stream.list) & runIdentity
            x === Right ('a', 'b', 'c')

        specify "(p <* end) fails on \"abcd\"" $ hedgehog do
            input :: [Text] <- forAll (genChunks "abcd")
            let x = input & evalStateT (P.parseOnly def (p P.<* end) Stream.list) & runIdentity
            x === Left (P.Error [])

    describe "p = contextualize \"Digit\" (require (satisfy isDigit))" do
        let p = P.contextualize "Digit" (P.satisfyJust \x -> if Char.isDigit x then Just x else Nothing)

        specify "p parses \"2\"" $ hedgehog do
            input :: [Text] <- forAll (genChunks "2")
            let x = input & evalStateT (P.parseOnly def p Stream.list) & runIdentity
            x === Right '2'

        specify "p fails on \"a\"" $ hedgehog do
            input :: [Text] <- forAll (genChunks "a")
            let x = input & evalStateT (P.parseOnly def p Stream.list) & runIdentity
            x === Left (P.Error ["Digit"])

    describe "p = repetition0 (satisfy isDigit)" do
        let p = P.repetition0 (P.satisfyJust \x -> if Char.isDigit x then Just x else Nothing)

        specify "p parses 123 from 123abc" $ hedgehog do
            input :: [Text] <- forAll (genChunks "123abc")
            let x = input & evalStateT (P.parseOnly def p Stream.list) & runIdentity
            x === Right ("123" :: [Char])

        specify "p parses nothing from abc" $ hedgehog do
            input :: [Text] <- forAll (genChunks "abc")
            let x = input & evalStateT (P.parseOnly def p Stream.list) & runIdentity
            x === Right ([] :: [Char])

    describe "p = text \"abc\"" do
        let p = P.text (NontrivialUnsafe "abc")

        specify "p parses \"abc\" and \"abcd\"" $ hedgehog do
            input :: [Text] <- forAll (Gen.element ["abc", "abcd"] >>= genChunks)
            let x = input & evalStateT (P.parseOnly def p Stream.list) & runIdentity
            x === Right ()

        specify "p fails on any input that does not start with abc" $ hedgehog do
            input :: [Text] <- forAll (Gen.element ["", "ab", "bc"] >>= genChunks)
            let x = input & evalStateT (P.parseOnly def p Stream.list) & runIdentity
            x === Left (P.Error [])

    describe "cursorPosition" do

        specify "starts at 0" $ hedgehog do
            input :: [Text] <- forAll (Gen.element ["", "a", "bc", "abc", "abcd"] >>= genChunks)
            let p = P.cursorPosition
            let x = input & evalStateT (P.parseOnly def p Stream.list) & runIdentity
            x === Right 0

        specify "increases" $ hedgehog do
            input :: [Text] <- forAll (genChunks (ListLike.replicate 10 'a'))
            n :: Natural <- forAll (Gen.integral (Range.linear 0 5))
            let p = P.do{ _ <- P.count0 n (P.satisfyJust Just); P.cursorPosition }
            let x = input & evalStateT (P.parseOnly def p Stream.list) & runIdentity
            x === Right (P.CursorPosition n)

        specify "increases twice" $ hedgehog do
            input :: [Text] <- forAll (genChunks (ListLike.replicate 10 'a'))
            n1 :: Natural <- forAll (Gen.integral (Range.linear 0 5))
            n2 :: Natural <- forAll (Gen.integral (Range.linear 0 5))
            let p = P.do{ _ <- P.count0 n1 (P.satisfyJust Just); _ <- P.count0 n2 (P.satisfyJust Just); P.cursorPosition }
            let x = input & evalStateT (P.parseOnly def p Stream.list) & runIdentity
            x === Right (P.CursorPosition (n1 + n2))

    describe "position" do

        specify "starts at 1:1" $ hedgehog do
            input :: [Text] <- forAll (Gen.element ["", "a", "bc", "abc", "abcd"] >>= genChunks)
            let p = P.position
            let x = input & evalStateT (P.parseOnly def p Stream.list) & runIdentity
            x === Right (Loc.loc 1 1)

        specify "column is incremented by char when input contains no line breaks" $ hedgehog do
            n :: Natural <- forAll (Gen.integral (Range.linear 0 5))
            let p = P.do{ _ <- P.count0 n (P.satisfyJust Just); P.position }
            input :: [Text] <- forAll (genChunks (ListLike.fromList ['a' .. 'z']))
            let x = input & evalStateT (P.parseOnly def p Stream.list) & runIdentity
            x === Right (Loc.loc 1 (fromIntegral $ n + 1))

        specify "line is incremented by char when input is line breaks" $ hedgehog do
            n :: Natural <- forAll (Gen.integral (Range.linear 0 5))
            let p = P.do{ _ <- P.count0 n (P.satisfyJust Just); P.position }
            input :: [Text] <- forAll (genChunks (ListLike.replicate 50 '\n'))
            let x = input & evalStateT (P.parseOnly def p Stream.list) & runIdentity
            x === Right (Loc.loc (fromIntegral $ 1 + n) 1)

        specify "both line and column increments" $ hedgehog do
            let genInputLine = Gen.text (Range.singleton 19) Gen.alpha <&> (<> "\n")
            input :: [Text] <- forAll (genChunks =<< times 10 genInputLine)
            n :: Natural <- forAll (Gen.integral (Range.linear 0 200))
            let p = P.do{ _ <- P.count0 n (P.satisfyJust Just); P.position }
            let x = input & evalStateT (P.parseOnly def p Stream.list) & runIdentity
            let (a, b) = n `quotRem` 20
            let l = Loc.loc (fromIntegral $ 1 + a) (fromIntegral $ 1 + b)
            x === Right l

    describe "withLocation" do

        let
            withLocation act =
              (\a x b -> (Loc.spanOrLocFromTo a b, x))
              P.<$> P.position P.<*> act P.<*> P.position

        specify "one-line example" $ hedgehog do
            let p = P.do{ P.text (NontrivialUnsafe "abc"); x <- withLocation (P.text (NontrivialUnsafe "def")); P.text (NontrivialUnsafe "ghi"); P.return x }
            input :: [Text] <- forAll (genChunks "abcdefghi")
            let x = input & evalStateT (P.parseOnly def p Stream.list) & runIdentity
            x === Right (SpanOrLoc.fromTo (loc 1 4) (loc 1 7), ())

        specify "second-line example" $ hedgehog do
            let p = P.do{ P.text (NontrivialUnsafe "xyz\nabc"); x <- withLocation (P.text (NontrivialUnsafe "def")); P.text (NontrivialUnsafe "ghi"); P.return x }
            input :: [Text] <- forAll (genChunks "xyz\nabcdefghi")
            let x = input & evalStateT (P.parseOnly def p Stream.list) & runIdentity
            x === Right (SpanOrLoc.fromTo (loc 2 4) (loc 2 7), ())

        specify "empty example" $ hedgehog do
            let p = P.do{ P.text (NontrivialUnsafe "abc"); x <- withLocation (P.return ()); P.text (NontrivialUnsafe "def"); P.return x }
            input :: [Text] <- forAll (genChunks "abcdef")
            let x = input & evalStateT (P.parseOnly def p Stream.list) & runIdentity
            x === Right (SpanOrLoc.loc (loc 1 4), ())

    -- describe "while" do

    --     specify "..." $ hedgehog do
    --         let p = match
