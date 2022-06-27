module Step.Document.Past (Past (..), empty, record, positionLens) where

import Step.Buffer.Base (Buffer)
import qualified Step.Buffer.Base as Buffer

import Step.Document.Position (LineNumber, Position (Position))
import qualified Step.Document.Position as Position

import qualified Map

import qualified ListLike

data Past text =
  Past
    { lineMap :: Map LineNumber (Buffer text)
    , lastCharacterWasCR :: Bool
    , position :: Position
    }

makeLensesFor [("position", "positionLens")] ''Past

empty :: Past text
empty =
  Past
    { lineMap = Map.empty
    , lastCharacterWasCR = False
    , position = Position.start
    }

record :: ListLike text Char => text -> Past text -> Past text
record x p =
  case ListLike.uncons x of
    Nothing -> p
    Just ('\r', x') -> record x' (recordCR p)
    Just ('\n', x') -> record x' (recordLF p)
    Just _ -> let (a, b) = ListLike.break (`elem` ['\r', '\n']) x in record b (recordOther a p)

recordCR :: ListLike text Char => Past text -> Past text
recordCR p =
  Past
    { lineMap = Map.alter (Just . (<> Buffer.singleton (ListLike.singleton '\r')) . fromMaybe Buffer.empty) (Position.line (position p)) (lineMap p)
    , lastCharacterWasCR = True
    , position = over Position.columnLens (+ 1) (position p)
    }

recordLF :: ListLike text Char => Past text -> Past text
recordLF p =
  Past
    { lineMap = Map.alter (Just . (<> Buffer.singleton (ListLike.singleton '\n')) . fromMaybe Buffer.empty) (Position.line (position p)) (lineMap p)
    , lastCharacterWasCR = False
    , position = Position{ Position.line = Position.line (position p) + 1, Position.column = 1 }
    }

recordOther :: ListLike text Char => text -> Past text -> Past text
recordOther x p =
  let
    newPosition =
        case lastCharacterWasCR p of
            True ->
                Position{ Position.line = Position.line (position p) + 1, Position.column = Position.ColumnNumber (fromIntegral (ListLike.length x) + 1) }
            False ->
                over Position.columnLens (+ Position.ColumnNumber (fromIntegral (ListLike.length x))) (position p)
  in
    Past
      { lineMap = Map.alter (Just . (<> Buffer.singleton x) . fromMaybe Buffer.empty) (Position.line newPosition) (lineMap p)
      , lastCharacterWasCR = False
      , position = newPosition
      }
