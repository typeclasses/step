{-# language DerivingStrategies #-}

module Step.Document.Lines
  (
    {- * The type -} LineHistory (..),
    {- * Optics -} cursorPositionLens, lineStartPositionLens, lineTrackerLens, afterCRLens,
    empty,
    {- * Finding location at a cursor -} CursorLocation (..), locateCursorInDocument,
    {- * Construction -} build,
    {- * Char class -} Char (..),
    {- * Feeding input -} record,
  ) where

import Step.Internal.Prelude

import Loc (Line, Loc, loc)

import qualified Map

import Step.Input.CursorPosition (CursorPosition)
import qualified Step.Input.CursorPosition as CursorPosition

import qualified Char

import qualified ListLike

import qualified Loc

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

build :: Char char => ListLike text char => [text] -> LineHistory
build xs = execState (traverse_ record xs) empty

class Eq a => Char a
  where
    carriageReturn :: a
    lineFeed :: a

instance Char Char.Char
  where
    carriageReturn = '\r'
    lineFeed = '\n'

record :: Monad m => Char char => ListLike text char => text -> StateT LineHistory m ()
record x =
    case ListLike.uncons x of
        Nothing -> return ()
        Just (c, x') | c == carriageReturn -> do
            recordCR
            record x'
        Just (c, x') | c == lineFeed -> do
            recordLF
            record x'
        Just _ -> do
            let (a, b) = ListLike.break (`elem` [carriageReturn, lineFeed]) x
            recordOther a
            record b

startNewLine :: Monad m => StateT LineHistory m ()
startNewLine = do
    l <- use lineTrackerLens
    let l' = l + 1
    cp <- use cursorPositionLens
    modifying lineStartPositionLens (Map.insert cp (fromIntegral (Loc.toNat l')))
    assign lineTrackerLens l'

recordCR :: Monad m => StateT LineHistory m ()
recordCR = do
    acr <- use afterCRLens
    when acr startNewLine
    modifying cursorPositionLens (CursorPosition.increase 1)
    assign afterCRLens True

recordLF :: Monad m => StateT LineHistory m ()
recordLF = do
    modifying cursorPositionLens (CursorPosition.increase 1)
    startNewLine
    assign afterCRLens False

recordOther :: Monad m => ListLike text char => text -> StateT LineHistory m ()
recordOther x = do
    acr <- use afterCRLens
    when acr startNewLine
    modifying cursorPositionLens (CursorPosition.increase (fromIntegral (ListLike.length x)))
    assign afterCRLens False
