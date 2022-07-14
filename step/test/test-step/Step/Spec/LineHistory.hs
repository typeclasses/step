{-# language OverloadedStrings #-}

module Step.Spec.LineHistory where

import Step.Internal.Prelude

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

import qualified Step.LineHistory.Base as LineHistory
import qualified Step.LineHistory.Build as LineHistory

spec :: SpecWith ()
spec = describe "Line history" do

    specify "example 1" $ hedgehog do
        input :: [Text] <- forAll (genChunks "Move\r\nTwo\rThree")
        let lh = LineHistory.build input
        LineHistory.locateCursorInDocument  0 lh === Just (LineHistory.CursorAt (loc 1 1))
        LineHistory.locateCursorInDocument  1 lh === Just (LineHistory.CursorAt (loc 1 2))
        LineHistory.locateCursorInDocument  2 lh === Just (LineHistory.CursorAt (loc 1 3))
        LineHistory.locateCursorInDocument  3 lh === Just (LineHistory.CursorAt (loc 1 4))
        LineHistory.locateCursorInDocument  4 lh === Just (LineHistory.CursorAt (loc 1 5))
        LineHistory.locateCursorInDocument  5 lh === Just (LineHistory.CursorAt (loc 1 6))
        LineHistory.locateCursorInDocument  6 lh === Just (LineHistory.CursorAt (loc 2 1))
        LineHistory.locateCursorInDocument  7 lh === Just (LineHistory.CursorAt (loc 2 2))
        LineHistory.locateCursorInDocument  8 lh === Just (LineHistory.CursorAt (loc 2 3))
        LineHistory.locateCursorInDocument  9 lh === Just (LineHistory.CursorAt (loc 2 4))
        LineHistory.locateCursorInDocument 10 lh === Just (LineHistory.CursorAt (loc 3 1))
        LineHistory.locateCursorInDocument 11 lh === Just (LineHistory.CursorAt (loc 3 2))
        LineHistory.locateCursorInDocument 12 lh === Just (LineHistory.CursorAt (loc 3 3))
        LineHistory.locateCursorInDocument 13 lh === Just (LineHistory.CursorAt (loc 3 4))
        LineHistory.locateCursorInDocument 14 lh === Just (LineHistory.CursorAt (loc 3 5))
        LineHistory.locateCursorInDocument 15 lh === Just (LineHistory.CursorAt (loc 3 6))
        LineHistory.locateCursorInDocument 16 lh === Nothing

    specify "example 2" $ hedgehog do
        input :: [Text] <- forAll (genChunks "ab\r")
        let lh = LineHistory.build input
        LineHistory.locateCursorInDocument  0 lh === Just (LineHistory.CursorAt (loc 1 1))
        LineHistory.locateCursorInDocument  1 lh === Just (LineHistory.CursorAt (loc 1 2))
        LineHistory.locateCursorInDocument  2 lh === Just (LineHistory.CursorAt (loc 1 3))
        LineHistory.locateCursorInDocument  3 lh === Just (LineHistory.CursorLocationNeedsMoreInput{ LineHistory.ifEndOfInput = loc 2 1 })
        LineHistory.locateCursorInDocument  4 lh === Nothing
