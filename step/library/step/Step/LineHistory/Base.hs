module Step.LineHistory.Base
  (
    LineHistory (..), cursorPositionLens, lineStartPositionLens, lineTrackerLens, afterCRLens,
    empty,
    CursorLocation (..), locateCursorInDocument,
  )
  where

import Step.Internal.Prelude

import Step.Buffer.Base (Buffer)
import qualified Step.Buffer.Base as Buffer

import Loc (Line, Loc)
import qualified Loc
import qualified Step.Document.Loc as Loc

import qualified IntMap
import IntMap (IntMap)

import qualified ListLike

import Step.CursorPosition.Base (CursorPosition)
import qualified Step.CursorPosition.Base as CursorPosition

data LineHistory text =
  LineHistory
    { lineStartPosition :: IntMap CursorPosition
    , lineTracker :: Line
    , cursorPosition :: CursorPosition
    , afterCR :: Bool
    }

data CursorLocation =
    CursorAt Loc
  | CursorLocationNeedsMoreInput -- ^ The cursor is at this location, but this location immediately follows a carriage return character at the end of the recorded history. There is an ambiguity in this situation. If the next character is a line feed, then this location will change to 'CursorAt'. If the next character is not a line feed, this location will change to 'CursorAtLineEnd'.

makeLensesFor
    [ ("cursorPosition", "cursorPositionLens")
    , ("lineStartPosition", "lineStartPositionLens")
    , ("lineTracker", "lineTrackerLens")
    , ("afterCR", "afterCRLens")
    ]
    ''LineHistory

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
    { lineStartPosition = IntMap.empty
    , lineTracker = 1
    , cursorPosition = CursorPosition.origin
    , afterCR = False
    }
