{-# language OverloadedStrings #-}

module Step.Spec.LineHistory where

import Step.Internal.Prelude

import Hedgehog

import qualified Hedgehog.Gen as Gen

import Step.Test.InputChunking (genChunks)

import Test.Hspec
import Test.Hspec.Hedgehog

import Text (Text)

import Loc (loc)

import qualified Map

import qualified Step.Document.Lines as Lines
import Step.Document.Lines (CursorLocation (..), locateCursorInDocument, LineHistory (..))

spec :: SpecWith ()
spec = describe "Line history" do

    specify "example 1" $ hedgehog do

        input :: [Text] <- forAll (genChunks "Move\r\nTwo\rThree")

        t <- forAll Gen.bool
        let lh = Lines.build input & (if t then execState Lines.terminate else id)

        locateCursorInDocument  0 lh === Just (CursorAt (loc 1 1))
        locateCursorInDocument  1 lh === Just (CursorAt (loc 1 2))
        locateCursorInDocument  2 lh === Just (CursorAt (loc 1 3))
        locateCursorInDocument  3 lh === Just (CursorAt (loc 1 4))
        locateCursorInDocument  4 lh === Just (CursorAt (loc 1 5))
        locateCursorInDocument  5 lh === Just (CursorAt (loc 1 6))
        locateCursorInDocument  6 lh === Just (CursorAt (loc 2 1))
        locateCursorInDocument  7 lh === Just (CursorAt (loc 2 2))
        locateCursorInDocument  8 lh === Just (CursorAt (loc 2 3))
        locateCursorInDocument  9 lh === Just (CursorAt (loc 2 4))
        locateCursorInDocument 10 lh === Just (CursorAt (loc 3 1))
        locateCursorInDocument 11 lh === Just (CursorAt (loc 3 2))
        locateCursorInDocument 12 lh === Just (CursorAt (loc 3 3))
        locateCursorInDocument 13 lh === Just (CursorAt (loc 3 4))
        locateCursorInDocument 14 lh === Just (CursorAt (loc 3 5))
        locateCursorInDocument 15 lh === Just (CursorAt (loc 3 6))
        locateCursorInDocument 16 lh === Nothing

    specify "example 2" $ hedgehog do

        input :: [Text] <- forAll (genChunks "ab\r")

        t <- forAll Gen.bool
        let lh = Lines.build input & (if t then execState Lines.terminate else id)

        locateCursorInDocument  0 lh === Just (CursorAt (loc 1 1))
        locateCursorInDocument  1 lh === Just (CursorAt (loc 1 2))
        locateCursorInDocument  2 lh === Just (CursorAt (loc 1 3))
        locateCursorInDocument  3 lh === Just
            (
                if t then CursorAt (loc 2 1) else CursorLocationNeedsMoreInput
            )
        locateCursorInDocument  4 lh === Nothing

    specify "example 3" $ hedgehog do
        let lh = LineHistory
                  { lineStartPosition = Map.fromList [(0, 1)]
                  , lineTracker = 1
                  , cursorPosition = 6
                  , afterCR = False
                  , terminated = False
                  }
        locateCursorInDocument 3 lh === Just (CursorAt (loc 1 4))

    specify "one-line example" $ hedgehog do

        input :: [Text] <- forAll (genChunks "abc")

        let lh = Lines.build input

        lh === LineHistory
                  { lineStartPosition = Map.fromList [(0, 1)]
                  , lineTracker = 1
                  , cursorPosition = 3
                  , afterCR = False
                  , terminated = False
                  }

        locateCursorInDocument  0 lh === Just (CursorAt (loc 1 1))
        locateCursorInDocument  1 lh === Just (CursorAt (loc 1 2))
        locateCursorInDocument  2 lh === Just (CursorAt (loc 1 3))
        locateCursorInDocument  3 lh === Just (CursorAt (loc 1 4))
        locateCursorInDocument  4 lh === Nothing
