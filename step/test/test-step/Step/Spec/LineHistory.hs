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

import qualified Step.Document.Lines as Lines

spec :: SpecWith ()
spec = describe "Line history" do

    specify "example 1" $ hedgehog do
        input :: [Text] <- forAll (genChunks "Move\r\nTwo\rThree")
        let lh = Lines.build input
        Lines.locateCursorInDocument  0 lh === Just (Lines.CursorAt (loc 1 1))
        Lines.locateCursorInDocument  1 lh === Just (Lines.CursorAt (loc 1 2))
        Lines.locateCursorInDocument  2 lh === Just (Lines.CursorAt (loc 1 3))
        Lines.locateCursorInDocument  3 lh === Just (Lines.CursorAt (loc 1 4))
        Lines.locateCursorInDocument  4 lh === Just (Lines.CursorAt (loc 1 5))
        Lines.locateCursorInDocument  5 lh === Just (Lines.CursorAt (loc 1 6))
        Lines.locateCursorInDocument  6 lh === Just (Lines.CursorAt (loc 2 1))
        Lines.locateCursorInDocument  7 lh === Just (Lines.CursorAt (loc 2 2))
        Lines.locateCursorInDocument  8 lh === Just (Lines.CursorAt (loc 2 3))
        Lines.locateCursorInDocument  9 lh === Just (Lines.CursorAt (loc 2 4))
        Lines.locateCursorInDocument 10 lh === Just (Lines.CursorAt (loc 3 1))
        Lines.locateCursorInDocument 11 lh === Just (Lines.CursorAt (loc 3 2))
        Lines.locateCursorInDocument 12 lh === Just (Lines.CursorAt (loc 3 3))
        Lines.locateCursorInDocument 13 lh === Just (Lines.CursorAt (loc 3 4))
        Lines.locateCursorInDocument 14 lh === Just (Lines.CursorAt (loc 3 5))
        Lines.locateCursorInDocument 15 lh === Just (Lines.CursorAt (loc 3 6))
        Lines.locateCursorInDocument 16 lh === Nothing

    specify "example 2" $ hedgehog do
        input :: [Text] <- forAll (genChunks "ab\r")
        let lh = Lines.build input
        Lines.locateCursorInDocument  0 lh === Just (Lines.CursorAt (loc 1 1))
        Lines.locateCursorInDocument  1 lh === Just (Lines.CursorAt (loc 1 2))
        Lines.locateCursorInDocument  2 lh === Just (Lines.CursorAt (loc 1 3))
        Lines.locateCursorInDocument  3 lh === Just (Lines.CursorLocationNeedsMoreInput{ Lines.ifEndOfInput = loc 2 1 })
        Lines.locateCursorInDocument  4 lh === Nothing
