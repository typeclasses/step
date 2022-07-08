{-# language DerivingStrategies #-}

module Step.LineHistory.Base
  (
    {- * The type -} LineHistory (..),
    {- * Optics -} cursorPositionLens, lineStartPositionLens, lineTrackerLens, afterCRLens,
    empty,
    {- * Finding location at a cursor -} CursorLocation (..), locateCursorInDocument,
  )
  where

import Step.Internal.Prelude

import Loc (Line, Loc, loc)

import qualified Map

import Step.CursorPosition.Base (CursorPosition)
import qualified Step.CursorPosition.Base as CursorPosition

data LineHistory =
  LineHistory
    { lineStartPosition :: Map CursorPosition Line
    , lineTracker :: Line
    , cursorPosition :: CursorPosition
    , afterCR :: Bool
    }

data CursorLocation =
    CursorAt Loc
  | CursorLocationNeedsMoreInput{ ifEndOfInput :: Loc } -- ^ The cursor is at this location, but this location immediately follows a carriage return character at the end of the recorded history. There is an ambiguity in this situation. If the next character is a line feed, then this location will change to 'CursorAt'. If the next character is not a line feed, this location will change to 'CursorAtLineEnd'.
  deriving stock (Eq, Show)

cursorPositionLens :: Lens' LineHistory CursorPosition
cursorPositionLens = lens cursorPosition \x y -> x{ cursorPosition = y }

lineStartPositionLens :: Lens' LineHistory (Map CursorPosition Line)
lineStartPositionLens = lens lineStartPosition \x y -> x{ lineStartPosition = y }

lineTrackerLens :: Lens' LineHistory Line
lineTrackerLens = lens lineTracker \x y -> x{ lineTracker = y }

afterCRLens :: Lens' LineHistory Bool
afterCRLens = lens afterCR \x y -> x{ afterCR = y }

locateCursorInDocument :: CursorPosition -> LineHistory -> Maybe CursorLocation
locateCursorInDocument cp lh | cp == cursorPosition lh && afterCR lh =
    Just $ CursorLocationNeedsMoreInput{ ifEndOfInput = loc l c }
  where
    l = 1 + lineTracker lh
    c = fromIntegral $ 1 + CursorPosition.absoluteDifference cp (cursorPosition lh)
locateCursorInDocument cp lh | cp > cursorPosition lh = Nothing
locateCursorInDocument cp lh =
    case Map.splitLookup cp (lineStartPosition lh) of
        (_, Just x, _) -> Just (CursorAt (loc x 1))
        (m, Nothing, _) -> case Map.lookupMax m of
            Nothing -> Nothing
            Just (cp', l) -> Just (CursorAt (loc l c))
              where
                c = fromIntegral (1 + CursorPosition.absoluteDifference cp cp')

empty :: LineHistory
empty =
  LineHistory
    { lineStartPosition = Map.singleton 0 1
    , lineTracker = 1
    , cursorPosition = CursorPosition.origin
    , afterCR = False
    }
