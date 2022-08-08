module Step.LineHistory
  (
    {- * The type -} LineHistory (..),
    {- * Terminators -} Terminators (..), charTerminators,
    {- * Optics -} cursorPositionLens, lineStartPositionLens, lineTrackerLens, afterCRLens,
    empty,
    {- * Finding location at a cursor -} CursorLocation (..), locateCursorInDocument,
    {- * Construction -} build,
    {- * Feeding input -} record, terminate,
  )
  where

import Step.Internal.Prelude

import Loc (Line, loc)

import qualified Map

import Step.CursorPosition (CursorPosition (..))
import qualified Step.CursorPosition as CursorPosition

import qualified Char

import qualified Loc

import Step.Nontrivial (Nontrivial, Span (..), SpanOperation (..))
import qualified Step.Nontrivial as Nontrivial

import qualified List

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

cursorDiff :: CursorPosition -> CursorPosition -> Natural
cursorDiff (CursorPosition a) (CursorPosition b) = if a >= b then a - b else b - a

locateCursorInDocument :: CursorPosition -> LineHistory -> Maybe CursorLocation

locateCursorInDocument cp lh | cp == cursorPosition lh && afterCR lh && terminated lh =
    Just $ CursorAt $ loc l c
  where
    l = 1 + lineTracker lh
    c = fromIntegral $ 1 + cursorDiff cp (cursorPosition lh)

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
                c = fromIntegral (1 + cursorDiff cp cp')

empty :: LineHistory
empty =
  LineHistory
    { lineStartPosition = Map.singleton 0 1
    , lineTracker = 1
    , cursorPosition = 0
    , afterCR = False
    , terminated = False
    }

build :: SpanOperation xs x -> Terminators x -> [Nontrivial xs x] -> LineHistory
build spanOp ts xs = runIdentity $ execRST (traverse_ (record spanOp) xs) ts empty

data Terminators x = Terminators{ isCarriageReturn :: Predicate x, isLineFeed :: Predicate x }

charTerminators :: Terminators Char.Char
charTerminators = Terminators
  { isCarriageReturn = Predicate $ (==) '\r'
  , isLineFeed = Predicate $ (==) '\n'
  }

notTerminator :: Terminators x -> Predicate x
notTerminator ts = Predicate \c ->
    not $ List.any @[]
        (\p -> getPredicate p c)
        [isCarriageReturn ts, isLineFeed ts]

record :: forall xs x m. Monad m =>
    SpanOperation xs x -> Nontrivial xs x -> RST (Terminators x) LineHistory m ()
record SpanOperation{ span } = fix \r x -> ask >>= \ts ->
  let
    h = Nontrivial.head x
    t = Nontrivial.tail x
  in
    if getPredicate (isCarriageReturn ts) h then recordCR *> traverse_ r t
    else if getPredicate (isLineFeed ts) h then recordLF *> traverse_ r t
    else
      do
        case span (notTerminator ts) x of
            SpanNone -> error "Lines.record"
            SpanAll -> recordOther x
            SpanPart{ spannedPart, spanRemainder } ->
                recordOther spannedPart *> r spanRemainder

terminate :: Monad m => RST r LineHistory m ()
terminate = modify' \x -> x{ terminated = True }

startNewLine :: Monad m => RST r LineHistory m ()
startNewLine = do
    l <- use lineTrackerLens
    let l' = l + 1
    cp <- use cursorPositionLens
    modifying lineStartPositionLens $
        Map.insert cp $ fromIntegral $ Loc.toNat l'
    assign lineTrackerLens l'

recordCR :: Monad m => RST r LineHistory m ()
recordCR = do
    acr <- use afterCRLens
    when acr startNewLine
    modifying cursorPositionLens $ appEndo $ CursorPosition.increase 1
    assign afterCRLens True

recordLF :: Monad m => RST r LineHistory m ()
recordLF = do
    modifying cursorPositionLens $ appEndo $ CursorPosition.increase 1
    startNewLine
    assign afterCRLens False

recordOther :: Monad m => Nontrivial xs x -> RST r LineHistory m ()
recordOther x = do
    acr <- use afterCRLens
    when acr startNewLine
    modifying cursorPositionLens $ appEndo $
        CursorPosition.strictlyIncrease $ Nontrivial.length x
    assign afterCRLens False
