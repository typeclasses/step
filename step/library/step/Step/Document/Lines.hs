module Step.Document.Lines
  (
    {- * The type -} LineHistory (..),
    {- * Optics -} cursorPositionLens, lineStartPositionLens, lineTrackerLens, afterCRLens,
    empty,
    {- * Finding location at a cursor -} CursorLocation (..), locateCursorInDocument,
    {- * Construction -} build,
    {- * Char class -} Char (..),
    {- * Feeding input -} record, terminate,
  ) where

import Step.Internal.Prelude

import Loc (Line, Loc, loc)

import qualified Map

import Step.Input.CursorPosition (CursorPosition)
import qualified Step.Input.CursorPosition as CursorPosition

import qualified Char

import qualified Loc

import Step.Nontrivial (Nontrivial)
import qualified Step.Nontrivial as Nontrivial

import Step.RST

data LineHistory =
  LineHistory
    { lineStartPosition :: Map CursorPosition Line
    , lineTracker :: Line
    , cursorPosition :: CursorPosition
    , afterCR :: Bool
    , terminated :: Bool
    }
  deriving stock (Eq, Show)

data CursorLocation =
    CursorAt Loc
  | CursorLocationNeedsMoreInput -- ^ The location immediately follows a carriage return character at the end of unterminated history. There is an ambiguity in this situation. To resolve it, feed more input using 'record' or 'terminate'.
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

locateCursorInDocument cp lh | cp == cursorPosition lh && afterCR lh && terminated lh =
    Just $ CursorAt $ loc l c
  where
    l = 1 + lineTracker lh
    c = fromIntegral $ 1 + CursorPosition.absoluteDifference cp (cursorPosition lh)

locateCursorInDocument cp lh | cp == cursorPosition lh && afterCR lh =
    Just CursorLocationNeedsMoreInput

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
    , terminated = False
    }

build :: Char x => [Nontrivial xs x] -> LineHistory
build xs = runIdentity $ execRST (traverse_ record xs) () empty

class Eq a => Char a
  where
    carriageReturn :: a
    lineFeed :: a

instance Char Char.Char
  where
    carriageReturn = '\r'
    lineFeed = '\n'

record :: forall xs x r m. Monad m => Char x => Nontrivial xs x -> RST r LineHistory m ()
record x =
  if Nontrivial.head x == carriageReturn then
      recordCR *> traverse_ record (Nontrivial.tail x)
  else if Nontrivial.head x == lineFeed then
      recordLF *> traverse_ record (Nontrivial.tail x)
  else
    do
      case Nontrivial.span x (Predicate \c -> not $ elem @[] c [carriageReturn, lineFeed]) of
          Nontrivial.SpanNone -> error "Lines.record"
          Nontrivial.SpanAll -> recordOther x
          Nontrivial.SpanPart{ Nontrivial.spannedPart, Nontrivial.spanRemainder } ->
              recordOther spannedPart *> record spanRemainder

terminate :: Monad m => RST r LineHistory m ()
terminate = modify' \x -> x{ terminated = True }

startNewLine :: Monad m => RST r LineHistory m ()
startNewLine = do
    l <- use lineTrackerLens
    let l' = l + 1
    cp <- use cursorPositionLens
    modifying lineStartPositionLens (Map.insert cp (fromIntegral (Loc.toNat l')))
    assign lineTrackerLens l'

recordCR :: Monad m => RST r LineHistory m ()
recordCR = do
    acr <- use afterCRLens
    when acr startNewLine
    modifying cursorPositionLens (CursorPosition.increase 1)
    assign afterCRLens True

recordLF :: Monad m => RST r LineHistory m ()
recordLF = do
    modifying cursorPositionLens (CursorPosition.increase 1)
    startNewLine
    assign afterCRLens False

recordOther :: Monad m => Nontrivial xs x -> RST r LineHistory m ()
recordOther x = do
    acr <- use afterCRLens
    when acr startNewLine
    modifying cursorPositionLens (CursorPosition.strictlyIncrease (Nontrivial.length x))
    assign afterCRLens False
