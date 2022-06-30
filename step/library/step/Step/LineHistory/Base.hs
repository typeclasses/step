module Step.LineHistory.Base
  (
    LineHistory (..),
    empty,
    record,
    locateCursorInDocument,
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

import Step.Cursor.Base (CursorPosition)

data LineHistory text =
  LineHistory
    { lineMap :: Map Line (Buffer text)
    , lastCharacterWasCR :: Bool
    , documentPosition :: Loc
    , cursorPosition :: CursorPosition
    }

locateCursorInDocument :: CursorPosition -> LineHistory text -> Maybe Loc
locateCursorInDocument = _

empty :: LineHistory text
empty =
  LineHistory
    { lineMap = Map.empty
    , lastCharacterWasCR = False
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
    { lineMap = Map.alter (Just . (<> Buffer.singleton (ListLike.singleton '\r')) . fromMaybe Buffer.empty) (Loc.locLine (documentPosition p)) (lineMap p)
    , lastCharacterWasCR = True
    , documentPosition = over Loc.columnLens (+ 1) (documentPosition p)
    , cursorPosition = cursorPosition p + 1
    }

recordLF :: ListLike text Char => LineHistory text -> LineHistory text
recordLF p =
  LineHistory
    { lineMap = Map.alter (Just . (<> Buffer.singleton (ListLike.singleton '\n')) . fromMaybe Buffer.empty) (Loc.locLine (documentPosition p)) (lineMap p)
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
                Loc.loc (Loc.locLine (documentPosition p) + 1) (fromIntegral (ListLike.length x) + 1)
            False ->
                over Loc.columnLens (+ fromIntegral (ListLike.length x)) (documentPosition p)
  in
    LineHistory
      { lineMap = Map.alter (Just . (<> Buffer.singleton x) . fromMaybe Buffer.empty) (Loc.locLine newPosition) (lineMap p)
      , lastCharacterWasCR = False
      , documentPosition = newPosition
      , cursorPosition = cursorPosition p + fromIntegral (ListLike.length x)
      }
