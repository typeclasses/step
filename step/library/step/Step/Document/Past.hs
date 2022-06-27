module Step.Document.Past (Past (..), empty, record, positionLens) where

import Step.Internal.Prelude

import Step.Buffer.Base (Buffer)
import qualified Step.Buffer.Base as Buffer

import Loc (Line, Loc)
import qualified Loc
import qualified Step.Document.Loc as Loc

import qualified Map

import qualified ListLike

data Past text =
  Past
    { lineMap :: Map Line (Buffer text)
    , lastCharacterWasCR :: Bool
    , position :: Loc
    }

makeLensesFor [("position", "positionLens")] ''Past

empty :: Past text
empty =
  Past
    { lineMap = Map.empty
    , lastCharacterWasCR = False
    , position = Loc.origin
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
    { lineMap = Map.alter (Just . (<> Buffer.singleton (ListLike.singleton '\r')) . fromMaybe Buffer.empty) (Loc.locLine (position p)) (lineMap p)
    , lastCharacterWasCR = True
    , position = over Loc.columnLens (+ 1) (position p)
    }

recordLF :: ListLike text Char => Past text -> Past text
recordLF p =
  Past
    { lineMap = Map.alter (Just . (<> Buffer.singleton (ListLike.singleton '\n')) . fromMaybe Buffer.empty) (Loc.locLine (position p)) (lineMap p)
    , lastCharacterWasCR = False
    , position = Loc.loc (Loc.locLine (position p) + 1) 1
    }

recordOther :: ListLike text Char => text -> Past text -> Past text
recordOther x p =
  let
    newPosition =
        case lastCharacterWasCR p of
            True ->
                Loc.loc (Loc.locLine (position p) + 1) (fromIntegral (ListLike.length x) + 1)
            False ->
                over Loc.columnLens (+ fromIntegral (ListLike.length x)) (position p)
  in
    Past
      { lineMap = Map.alter (Just . (<> Buffer.singleton x) . fromMaybe Buffer.empty) (Loc.locLine newPosition) (lineMap p)
      , lastCharacterWasCR = False
      , position = newPosition
      }
