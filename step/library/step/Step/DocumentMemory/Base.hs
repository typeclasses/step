{-# language FlexibleContexts, FlexibleInstances, TypeFamilies #-}

module Step.DocumentMemory.Base where

import Step.Internal.Prelude hiding (Text)

import Step.LineHistory.Char (Char)
import Step.LineHistory.Base (LineHistory)
import qualified Step.LineHistory.Base as LineHistory
import qualified Step.LineHistory.State as LineHistory.State

import Step.Cursor.Base (Cursor)
import qualified Step.Cursor.Base as Cursor

import Step.CursorPosition.Base (CursorPosition)

import qualified Step.Cursor.State as Cursor.State

import Loc (Loc)

import qualified Step.Classes as Class

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

runCursorState :: Monad m =>
    StateT (DocumentCursor text m) (StateT LineHistory m) a ->
    StateT (DocumentMemory text m) m a
runCursorState go = do
    dm <- get
    let co = content dm
    let cu = cursor dm
    ((x, cu'), co') <- lift $ runStateT (runStateT go cu) co
    put DocumentMemory{ cursor = cu', content = co' }
    return x

instance Monad m => Class.Peek1 (StateT (DocumentMemory text m) m) where
    type Text (StateT (DocumentMemory text m) m) = text
    peekCharMaybe = runCursorState Cursor.State.peekCharMaybe
    atEnd = runCursorState Cursor.State.atEnd

instance Monad m => Class.Locating (StateT (DocumentMemory text m) m) where
    position = attempt1
      where
        attempt1 = use (to position) >>= \case
            CursorAt x -> return x
            CursorLocationNeedsMoreInput -> runCursorState Cursor.State.bufferMore *> attempt2
        attempt2 = use (to position) <&> \case
            CursorAt x -> x
            CursorLocationNeedsMoreInput -> error "position @DocumentMemory" -- after buffering more, should not need more input to determine position

instance Monad m => Class.Take1 (StateT (DocumentMemory text m) m) where
    considerChar f = runCursorState (Cursor.State.considerChar f)

instance Monad m => Class.TakeAll (StateT (DocumentMemory text m) m) where
    takeAll = runCursorState Cursor.State.takeAll

instance (Monad m, Eq text) => Class.SkipTextNonAtomic (StateT (DocumentMemory text m) m) where
    skipTextNonAtomic x = runCursorState (Cursor.State.takeTextNotAtomic x)
