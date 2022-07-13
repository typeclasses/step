module Step.DocumentMemory.Base where

import Step.Internal.Prelude

import Step.LineHistory.Char (Char)
import Step.LineHistory.Base (LineHistory)
import qualified Step.LineHistory.Base as LineHistory
import qualified Step.LineHistory.State as LineHistory.State

import Step.Cursor.Base (Cursor)
import qualified Step.Cursor.Base as Cursor

import Step.CursorPosition.Base (CursorPosition)

import Loc (Loc)

data DocumentMemory text m =
  DocumentMemory
    { content :: LineHistory
    , cursor :: Cursor (StateT LineHistory m) text
    }

data CursorLocation =
    CursorAt Loc
  | CursorLocationNeedsMoreInput

contentLens :: Lens' (DocumentMemory text m) LineHistory
contentLens = lens content \x y -> x{ content = y }

cursorLens :: Lens
    (DocumentMemory text1 m1)
    (DocumentMemory text2 m2)
    (Cursor (StateT LineHistory m1) text1)
    (Cursor (StateT LineHistory m2) text2)
cursorLens = lens cursor \x y -> x{ cursor = y }

type DocumentCursor text m = Cursor (StateT LineHistory m) text

fromListT :: Char char => ListLike text char => Monad m => ListT m text -> DocumentMemory text m
fromListT xs =
  DocumentMemory
    { content = LineHistory.empty
    , cursor = Cursor.fromListT $ recordStream (execState . LineHistory.State.record) xs
    }

cursorPositionLens :: Lens' (DocumentMemory text m) (CursorPosition)
cursorPositionLens = cursorLens % Cursor.positionLens

cursorPosition :: DocumentMemory text m -> CursorPosition
cursorPosition x = Cursor.position (cursor x)

position :: DocumentMemory text m -> CursorLocation
position x = case LineHistory.locateCursorInDocument (cursorPosition x) (content x) of
    Just l -> case l of
        LineHistory.CursorLocationNeedsMoreInput{ LineHistory.ifEndOfInput = i } ->
            if isAllBuffered x then CursorAt i else CursorLocationNeedsMoreInput
        LineHistory.CursorAt l' -> CursorAt l'
    Nothing -> error "invalid DocumentMemory"

isAllBuffered :: DocumentMemory text m -> Bool
isAllBuffered = Cursor.isAllBuffered . cursor
