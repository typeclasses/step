{-# language OverloadedStrings #-}

module Step.Spec.LineHistory where

import Step.Internal.Prelude

import Hedgehog

import qualified Hedgehog.Gen as Gen

import Step.Cursor (genChunks)

import Test.Tasty
import qualified Test.Tasty.Hedgehog as Hedgehog

import Text (Text)

import Loc (loc)

import qualified Map

import qualified Step.LineHistory as Lines
import Step.LineHistory (CursorLocation (..), locateCursorInDocument, LineHistory (..))

import Step.Nontrivial (Nontrivial)
import qualified Step.Nontrivial.ListLike as NT

import Char (Char)

import Step.RST

type TextChunks = [Nontrivial Text Char]

tests :: TestTree
tests = testGroup "LineHistory"
  [ Hedgehog.testPropertyNamed "example 1" "prop_ex1" prop_ex1
  , Hedgehog.testPropertyNamed "example 2" "prop_ex1" prop_ex2
  , Hedgehog.testPropertyNamed "example 3" "prop_ex1" prop_ex3
  , Hedgehog.testPropertyNamed "one-line example" "prop_oneLine" prop_oneLine
  ]

prop_ex1 = property do
    input :: TextChunks <- forAll (genChunks "Move\r\nTwo\rThree")

    t <- forAll Gen.bool
    let lh = Lines.build NT.spanOperation NT.leftViewOperation Lines.charTerminators input & (if t then runIdentity . execRST Lines.terminate () else id)

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

prop_ex2 = property do
    input :: TextChunks <- forAll (genChunks "ab\r")

    t <- forAll Gen.bool
    let lh = Lines.build NT.spanOperation NT.leftViewOperation Lines.charTerminators input & (if t then runIdentity . execRST Lines.terminate () else id)

    locateCursorInDocument  0 lh === Just (CursorAt (loc 1 1))
    locateCursorInDocument  1 lh === Just (CursorAt (loc 1 2))
    locateCursorInDocument  2 lh === Just (CursorAt (loc 1 3))
    locateCursorInDocument  3 lh === Just
        (
            if t then CursorAt (loc 2 1) else CursorLocationNeedsMoreInput
        )
    locateCursorInDocument  4 lh === Nothing

prop_ex3 = property do
    let lh = LineHistory
              { lineStartPosition = Map.fromList [(0, 1)]
              , lineTracker = 1
              , cursorPosition = 6
              , afterCR = False
              , terminated = False
              }
    locateCursorInDocument 3 lh === Just (CursorAt (loc 1 4))

prop_oneLine = property do
    input :: TextChunks <- forAll (genChunks "abc")

    let lh = Lines.build NT.spanOperation NT.leftViewOperation Lines.charTerminators input

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
