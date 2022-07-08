{-# language TemplateHaskell #-}

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
    { content :: LineHistory text
    , cursor :: Cursor (StateT (LineHistory text) m) text
    }

data CursorLocation =
    CursorAt Loc
  | CursorLocationNeedsMoreInput

makeLensesFor
    [ ("content", "contentLens")
    , ("cursor", "cursorLens")
    ]
    ''DocumentMemory

type DocumentCursor text m = Cursor (StateT (LineHistory text) m) text

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
