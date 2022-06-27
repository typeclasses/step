module Stratoparsec.Document.Past (Past (..), empty, record, positionLens) where

import Optics

import Stratoparsec.Buffer.Base (Buffer)
import qualified Stratoparsec.Buffer.Base as Buffer

import Stratoparsec.Document.Position (LineNumber, Position (Position))
import qualified Stratoparsec.Document.Position as Position

import qualified Map
import qualified Text

import Prelude hiding (empty)

data Past =
  Past
    { lineMap :: Map LineNumber (Buffer Text)
    , lastCharacterWasCR :: Bool
    , position :: Position
    }

makeLensesFor [("position", "positionLens")] ''Past

empty :: Past
empty =
  Past
    { lineMap = Map.empty
    , lastCharacterWasCR = False
    , position = Position.start
    }

record :: Text -> Past -> Past
record x p =
  case Text.uncons x of
    Nothing -> p
    Just ('\r', x') -> record x' (recordCR p)
    Just ('\n', x') -> record x' (recordLF p)
    Just _ -> let (a, b) = Text.break (`elem` ['\r', '\n']) x in record b (recordOther a p)

recordCR :: Past -> Past
recordCR p =
  Past
    { lineMap = Map.alter (Just . (<> Buffer.singleton "\r") . fromMaybe Buffer.empty) (Position.line (position p)) (lineMap p)
    , lastCharacterWasCR = True
    , position = over Position.columnLens (+ 1) (position p)
    }

recordLF :: Past -> Past
recordLF p =
  Past
    { lineMap = Map.alter (Just . (<> Buffer.singleton "\n") . fromMaybe Buffer.empty) (Position.line (position p)) (lineMap p)
    , lastCharacterWasCR = False
    , position = Position{ Position.line = Position.line (position p) + 1, Position.column = 0 }
    }

recordOther :: Text -> Past -> Past
recordOther x p =
  let
    newPosition =
        case lastCharacterWasCR p of
            True ->
                Position{ Position.line = Position.line (position p) + 1, Position.column = Position.ColumnNumber (fromIntegral (Text.length x)) }
            False ->
                over Position.columnLens (+ Position.ColumnNumber (fromIntegral (Text.length x))) (position p)
  in
    Past
      { lineMap = Map.alter (Just . (<> Buffer.singleton x) . fromMaybe Buffer.empty) (Position.line newPosition) (lineMap p)
      , lastCharacterWasCR = False
      , position = newPosition
      }
