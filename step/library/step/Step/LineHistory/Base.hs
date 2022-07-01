module Step.LineHistory.Base
  (
    LineHistory (..), empty, record,
    CursorLocation (..), locateCursorInDocument,
  )
  where

import Step.Internal.Prelude

import Step.Buffer.Base (Buffer)
import qualified Step.Buffer.Base as Buffer

import Loc (Line, Loc)
import qualified Loc
import qualified Step.Document.Loc as Loc

import qualified Map

import qualified ListLike

import Step.CursorPosition.Base (CursorPosition)
import qualified Step.CursorPosition.Base as CursorPosition

data LineHistory text =
  LineHistory
    { lineMap :: Map Line (CursorPosition, Buffer text)
    , lastCharacterLineEnder :: Maybe LineEndingChar
    , documentPosition :: Loc
    , cursorPosition :: CursorPosition
    }

data LineEndingChar = CR | LF

data CursorLocation =
    CursorAt Loc
  | CursorAtLineEnd Loc -- ^ The cursor is at this location, but since this location is the end of a line, it may also be considered to be at the start of the following line.
  | CursorAmbiguouslyAfterCR Loc -- ^ The cursor is at this location, but this location immediately follows a carriage return character at the end of the recorded history. There is an ambiguity in this situation. If the next character is a line feed, then this location will change to 'CursorAt'. If the next character is not a line feed, this location will change to 'CursorAtLineEnd'.

locateCursorInDocument :: CursorPosition -> LineHistory text -> Maybe CursorLocation
locateCursorInDocument cp lh = _

-- if cp == 0 then Just (CursorAt Loc.origin) else
--     if cp == cursorPosition lh then Just ((if lastCharacterWasCR then CursorAmbiguouslyAfterCR else CursorJust (documentPosition lh)) else
--     Map.lookupMax (Map.filter (\(cp', _) -> cp' <= cp) (lineMap lh))
--     <&> \(l, (cp', _)) ->
--         Loc.loc l (fromIntegral (1 + CursorPosition.absoluteDifference cp cp'))

empty :: LineHistory text
empty =
  LineHistory
    { lineMap = Map.empty
    , lastCharacterLineEnder = Nothing
    , documentPosition = Loc.origin
    , cursorPosition = 0
    }

record :: ListLike text Char => text -> LineHistory text -> LineHistory text
record x p =
  case ListLike.uncons x of
    Nothing -> p
    Just ('\r', x') -> record x' (recordCR p)
    Just ('\n', x') -> record x' (recordLF p)
    Just _ -> let (a, b) = ListLike.break (`elem` ['\r', '\n']) x in record b (recordOther a p)

recordCR :: ListLike text Char => LineHistory text -> LineHistory text
recordCR p =
  LineHistory
    { lineMap = Map.alter
          (let y = Buffer.singleton (ListLike.singleton '\r') in Just . \case
              Nothing -> (cursorPosition p, y)
              Just (cp, x) -> (cp, x <> y)
          )
          (Loc.locLine (documentPosition p)) (lineMap p)
    , lastCharacterWasCR = True
    , documentPosition = over Loc.columnLens (+ 1) (documentPosition p)
    , cursorPosition = cursorPosition p + 1
    }

recordLF :: ListLike text Char => LineHistory text -> LineHistory text
recordLF p =
  LineHistory
    { lineMap = Map.alter
          (let y = Buffer.singleton (ListLike.singleton '\n') in Just . \case
              Nothing -> (cursorPosition p, y)
              Just (cp, x) -> (cp, x <> y)
          )
          (Loc.locLine (documentPosition p)) (lineMap p)
    , lastCharacterWasCR = False
    , documentPosition = Loc.loc (Loc.locLine (documentPosition p) + 1) 1
    , cursorPosition = cursorPosition p + 1
    }

recordOther :: ListLike text Char => text -> LineHistory text -> LineHistory text
recordOther x p =
  let
    newPosition =
        case lastCharacterWasCR p of
            True ->
                Loc.loc (Loc.locLine (documentPosition p) + 1) (fromIntegral (ListLike.length x + 1))
            False ->
                (
                  if ListLike.null x then id else
                  over Loc.columnLens (+ fromIntegral (ListLike.length x))
                )
                (documentPosition p)
  in
    LineHistory
      { lineMap = Map.alter
          (let y = Buffer.singleton x in Just . \case
              Nothing -> (cursorPosition p, y)
              Just (cp, d) -> (cp, d <> y)
          )
            (Loc.locLine newPosition) (lineMap p)
      , lastCharacterWasCR = False
      , documentPosition = newPosition
      , cursorPosition = cursorPosition p + fromIntegral (ListLike.length x)
      }
