{-# language FlexibleContexts, FlexibleInstances, TypeFamilies #-}

module Step.Document.Memory
  (
    {- * The type -} DocumentMemory,
    {- * Construction -} fromListT,
    {- * Cursor location -} position, CursorLocation,
  )
  where

import Step.Internal.Prelude

import Step.Document.Lines (Char, LineHistory)
import qualified Step.Document.Lines as Lines

import Step.Input.Cursor (Cursor)
import qualified Step.Input.Cursor as Cursor

import qualified Step.Input.BufferedStream as BufferedStream

import Step.Input.CursorPosition (CursorPosition)

import Loc (Loc)

import qualified Step.Classes.Base as Class


-- The type

data DocumentMemory text m =
  DocumentMemory
    { content :: LineHistory
    , cursor :: Cursor (StateT LineHistory m) text
    }

instance Monad m => Class.Peek1 (StateT (DocumentMemory text m) m) where
    type Text (StateT (DocumentMemory text m) m) = text
    peekCharMaybe = runCursorState Class.peekCharMaybe
    atEnd = runCursorState Class.atEnd

instance Monad m => Class.Locating (StateT (DocumentMemory text m) m) where
    position = attempt1
      where
        attempt1 = use (to position) >>= \case
            CursorAt x -> return x
            CursorLocationNeedsMoreInput -> Class.bufferMore *> attempt2
        attempt2 = use (to position) <&> \case
            CursorAt x -> x
            CursorLocationNeedsMoreInput -> error "position @DocumentMemory" -- after buffering more, should not need more input to determine position

instance Monad m => Class.Take1 (StateT (DocumentMemory text m) m) where
    considerChar f = runCursorState (Class.considerChar f)

instance Monad m => Class.TakeAll (StateT (DocumentMemory text m) m) where
    takeAll = runCursorState Class.takeAll

instance (Monad m, Eq text) => Class.SkipTextNonAtomic (StateT (DocumentMemory text m) m) where
    skipTextNonAtomic x = runCursorState (Class.skipTextNonAtomic x)

instance Monad m => Class.FillBuffer1 (StateT (DocumentMemory text m) m) where
    fillBuffer1 = runCursorState Class.fillBuffer1

instance Monad m => Class.BufferMore (StateT (DocumentMemory text m) m) where
    bufferMore = runCursorState Class.bufferMore


-- Internal

runCursorState :: Monad m =>
    StateT (Cursor (StateT LineHistory m) text) (StateT LineHistory m) a ->
    StateT (DocumentMemory text m) m a
runCursorState go = do
    dm <- get
    let co = content dm
    let cu = cursor dm
    ((x, cu'), co') <- lift $ runStateT (runStateT go cu) co
    put DocumentMemory{ cursor = cu', content = co' }
    return x


-- Construction

fromListT :: Char char => ListLike text char => Monad m => ListT m text -> DocumentMemory text m
fromListT xs =
  DocumentMemory
    { content = Lines.empty
    , cursor = Cursor.fromListT $ recordStream (execState . Lines.record) xs
    }


-- Cursor location

data CursorLocation =
    CursorAt Loc
  | CursorLocationNeedsMoreInput

position :: DocumentMemory text m -> CursorLocation
position x = case Lines.locateCursorInDocument (Cursor.position (cursor x)) (content x) of
    Just l -> case l of
        Lines.CursorLocationNeedsMoreInput{ Lines.ifEndOfInput = i } ->
            if BufferedStream.isAllBuffered (Cursor.bufferedStream (cursor x)) then CursorAt i else CursorLocationNeedsMoreInput
        Lines.CursorAt l' -> CursorAt l'
    Nothing -> error "invalid DocumentMemory"
