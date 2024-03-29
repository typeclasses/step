{-# language FlexibleContexts, QualifiedDo, OverloadedStrings #-}

module Step.Spec.DocumentParsing (tests) where

import Step.Internal.Prelude

import Char (Char)
import Hedgehog
import Loc (loc)
import Test.Tasty
import Test.Tasty.Hedgehog
import Text (Text)
import Step.Cursor (genBlocks)
import Step.Nontrivial.Unsafe (Nontrivial (NontrivialUnsafe))

import qualified Char
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Integer
import qualified ListLike
import qualified Loc
import qualified SpanOrLoc
import qualified Step.Cursor as Cursor
import qualified Step.Document as P
import qualified Text

type TextChunks = [Nontrivial Text Char]

tests :: TestTree
tests = testGroup "Document-parsing tests"
  [ charCharCharTests
  , contextualizeTests
  , repetitionTests
  , cursorPositionTests
  , linePositionTests
  ]

charCharCharTests = testGroup "p = char, char, char"
  [ testPropertyNamed "(p <* end) parses \"abc\"" "prop_charCharChar1" prop_charCharChar1
  , testPropertyNamed "p parses \"abcd\"""prop_charCharChar2" prop_charCharChar2
  , testPropertyNamed "(p <* end) fails on \"abcd\"" "prop_charCharChar3" prop_charCharChar3
  ]

charCharChar = P.do{ a <- P.satisfyJust Just; b <- P.satisfyJust Just; c <- P.satisfyJust Just; P.return (a, b, c) }

prop_charCharChar1 = property do
    input :: TextChunks <- forAll (genBlocks "abc")
    let x = P.parseSimple (charCharChar P.<* P.end) input
    x === Right ('a', 'b', 'c')

prop_charCharChar2 = property do
    input :: TextChunks <- forAll (genBlocks "abcd")
    let x = P.parseSimple charCharChar input
    x === Right ('a', 'b', 'c')

prop_charCharChar3 = property do
    input :: TextChunks <- forAll (genBlocks "abcd")
    let x = P.parseSimple (charCharChar P.<* P.end) input
    x === Left (P.DocError [])

contextualizeTests = testGroup "p = contextualize \"Digit\" (require (satisfy isDigit))"
  [ testPropertyNamed "p parses \"2\"" "prop_digit2" prop_digit2
  , testPropertyNamed "p fails on \"a\"" "prop_digitA" prop_digitA
  ]

digit = P.contextualize "Digit" (P.satisfyJust \x -> if Char.isDigit x then Just x else Nothing)

prop_digit2 = property do
    input :: TextChunks <- forAll (genBlocks "2")
    let x = P.parseSimple digit input
    x === Right '2'

prop_digitA = property do
    input :: TextChunks <- forAll (genBlocks "a")
    let x = P.parseSimple digit input
    x === Left (P.DocError ["Digit"])

repetitionTests = testGroup "p = repetition0 (satisfy isDigit)"
  [ testPropertyNamed "p parses 123 from 123abc" "prop_digitList_part" prop_digitList_part
  , testPropertyNamed "p parses nothing from abc" "prop_digitList_nothing" prop_digitList_nothing
  ]

digitList = P.repetition0 (P.satisfyJust \x -> if Char.isDigit x then Just x else Nothing)

prop_digitList_part = property do
    input :: TextChunks <- forAll (genBlocks "123abc")
    let x = P.parseSimple digitList input
    x === Right ("123" :: [Char])

prop_digitList_nothing = property do
    input :: TextChunks <- forAll (genBlocks "abc")
    let x = P.parseSimple digitList input
    x === Right ([] :: [Char])

-- describe "p = text \"abc\"" do
--     let p = P.text (NontrivialUnsafe "abc")

--     specify "p parses \"abc\" and \"abcd\"" $ hedgehog do
--         input :: TextChunks <- forAll (Gen.element ["abc", "abcd"] >>= genBlocks)
--         let x = input & evalStateT (P.parseOnly def p Cursor.list) & runIdentity
--         x === Right ()

--     specify "p fails on any input that does not start with abc" $ hedgehog do
--         input :: TextChunks <- forAll (Gen.element ["", "ab", "bc"] >>= genBlocks)
--         let x = input & evalStateT (P.parseOnly def p Cursor.list) & runIdentity
--         x === Left (P.Error [])

cursorPositionTests = testGroup "cursorPosition"
  [ testPropertyNamed "starts at 0" "prop_cursorPosition_origin" prop_cursorPosition_origin
  , testPropertyNamed "increases" "prop_cursorPosition_increases" prop_cursorPosition_increases
  , testPropertyNamed "increases twice" "prop_cursorPosition_increases2" prop_cursorPosition_increases2
  ]

prop_cursorPosition_origin = property do
    input :: TextChunks <- forAll (Gen.element ["", "a", "bc", "abc", "abcd"] >>= genBlocks)
    let p = P.cursorPosition
    let x = P.parseSimple p input
    x === Right 0

prop_cursorPosition_increases = property do
    input :: TextChunks <- forAll (genBlocks (ListLike.replicate 10 'a'))
    n :: Natural <- forAll (Gen.integral (Range.linear 0 5))
    let p = P.do{ _ <- P.count0 n (P.satisfyJust Just); P.cursorPosition }
    let x = P.parseSimple p input
    x === Right (P.CursorPosition n)

prop_cursorPosition_increases2 = property do
    input :: TextChunks <- forAll (genBlocks (ListLike.replicate 10 'a'))
    n1 :: Natural <- forAll (Gen.integral (Range.linear 0 5))
    n2 :: Natural <- forAll (Gen.integral (Range.linear 0 5))
    let p = P.do{ _ <- P.count0 n1 (P.satisfyJust Just); _ <- P.count0 n2 (P.satisfyJust Just); P.cursorPosition }
    let x = P.parseSimple p input
    x === Right (P.CursorPosition (n1 + n2))

linePositionTests = testGroup "Line position"
  [ testPropertyNamed "starts at 1:1" "prop_linePosition_origin" prop_linePosition_origin
  , testPropertyNamed "column is incremented by char when input contains no line breaks" "prop_linePosition_oneLine" prop_linePosition_oneLine
  , testPropertyNamed "line is incremented by char when input is line breaks" "prop_linePosition_oneColumn" prop_linePosition_oneColumn
  , testPropertyNamed "both line and column increments" "prop_linePosition_both" prop_linePosition_both
  ]

prop_linePosition_origin = property do
    input :: TextChunks <- forAll (Gen.element ["", "a", "bc", "abc", "abcd"] >>= genBlocks)
    let p = P.position
    let x = P.parseSimple p input
    x === Right (Loc.loc 1 1)

prop_linePosition_oneLine = property do
    n :: Natural <- forAll (Gen.integral (Range.linear 0 5))
    atomic <- forAll Gen.bool
    let p = if atomic
        then P.castTo @P.Any P.do{ P.skipAtomically0 n; P.position }
        else P.do{ P.skip0 n; P.position }
    input :: TextChunks <- forAll (genBlocks (ListLike.fromList ['a' .. 'z']))
    let x = P.parseSimple p input
    x === Right (Loc.loc 1 (Integer.yolo $ n + 1))

prop_linePosition_oneColumn = property do
    n :: Natural <- forAll (Gen.integral (Range.linear 0 5))
    atomic <- forAll Gen.bool
    let p = if atomic
            then P.castTo @P.Any P.do{ P.skipAtomically0 n; P.position }
            else P.do{ P.skip0 n; P.position }
    input :: TextChunks <- forAll (genBlocks (ListLike.replicate 50 '\n'))
    let x = P.parseSimple p input
    x === Right (Loc.loc (Integer.yolo $ 1 + n) 1)

prop_linePosition_both = property do
    input :: TextChunks <- forAll $ genBlocks $ times 10 $ Text.replicate 19 "a" <> "\n"
    n :: Natural <- forAll (Gen.integral (Range.linear 0 200))
    atomic <- forAll Gen.bool
    let p = if atomic
            then P.castTo @P.Any P.do{ P.skipAtomically0 n; P.position }
            else P.do{ P.skip0 n; P.position }
    let x = P.parseSimple p input
    let (a, b) = n `quotRem` 20
    let l = Loc.loc (Integer.yolo $ 1 + a) (Integer.yolo $ 1 + b)
    x === Right l

    -- describe "withLocation" do

        -- let
        --     withLocation act =
        --       (\a x b -> (Loc.spanOrLocFromTo a b, x))
        --       P.<$> P.position P.<*> act P.<*> P.position

        -- specify "one-line example" $ hedgehog do
        --     let p = P.do{ P.text (NontrivialUnsafe "abc"); x <- withLocation (P.text (NontrivialUnsafe "def")); P.text (NontrivialUnsafe "ghi"); P.return x }
        --     input :: TextChunks <- forAll (genBlocks "abcdefghi")
        --     let x = input & evalStateT (P.parseOnly def p Cursor.list) & runIdentity
        --     x === Right (SpanOrLoc.fromTo (loc 1 4) (loc 1 7), ())

        -- specify "second-line example" $ hedgehog do
        --     let p = P.do{ P.text (NontrivialUnsafe "xyz\nabc"); x <- withLocation (P.text (NontrivialUnsafe "def")); P.text (NontrivialUnsafe "ghi"); P.return x }
        --     input :: TextChunks <- forAll (genBlocks "xyz\nabcdefghi")
        --     let x = input & evalStateT (P.parseOnly def p Cursor.list) & runIdentity
        --     x === Right (SpanOrLoc.fromTo (loc 2 4) (loc 2 7), ())

        -- specify "empty example" $ hedgehog do
        --     let p = P.do{ P.text (NontrivialUnsafe "abc"); x <- withLocation (P.return ()); P.text (NontrivialUnsafe "def"); P.return x }
        --     input :: TextChunks <- forAll (genBlocks "abcdef")
        --     let x = input & evalStateT (P.parseOnly def p Cursor.list) & runIdentity
        --     x === Right (SpanOrLoc.loc (loc 1 4), ())

    -- describe "while" do

    --     specify "..." $ hedgehog do
    --         let p = match
