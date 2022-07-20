{-# language FlexibleContexts, FlexibleInstances, FunctionalDependencies, TypeFamilies #-}

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

import Step.Input.BufferedStream (BufferedStream)
import qualified Step.Input.BufferedStream as BufferedStream

import Step.Input.CursorPosition (CursorPosition)

import Loc (Loc)

import qualified Step.Classes.Base as Class

import Step.Advancement (AdvanceResult, Progressive (..))
import qualified Step.Advancement as Advance

import Step.LookingAhead (Prophetic (forecast))
import qualified Step.LookingAhead (Prophetic (..))


-- The type

data DocumentMemory text char m =
  DocumentMemory
    { content :: LineHistory
    , cursor :: Cursor (BufferedStream (StateT LineHistory m) text char)
    }

instance (ListLike text char, Monad m) => Prophetic (StateT (DocumentMemory text char m) m) where
    type Text (StateT (DocumentMemory text char m) m) = text
    type Char (StateT (DocumentMemory text char m) m) = char
    forecast = changeBaseListT runCursorState forecast

instance (ListLike text char, Monad m) => Progressive (StateT (DocumentMemory text char m) m) where
    advance n = runCursorState (advance n)

instance (ListLike text char, Monad m) =>
    Class.Char1 (StateT (DocumentMemory text char m) m)
  where
    type Text (StateT (DocumentMemory text char m) m) = text
    type Char (StateT (DocumentMemory text char m) m) = char
    peekCharMaybe = runCursorState Class.peekCharMaybe
    atEnd = runCursorState Class.atEnd
    considerChar f = runCursorState (Class.considerChar f)

instance (ListLike text char, Monad m) => Class.Locating (StateT (DocumentMemory text char m) m) where
    position = attempt1
      where
        attempt1 = use (to position) >>= \case
            CursorAt x -> return x
            CursorLocationNeedsMoreInput -> Class.bufferMore *> attempt2
        attempt2 = use (to position) <&> \case
            CursorAt x -> x
            CursorLocationNeedsMoreInput -> error "position @DocumentMemory" -- after buffering more, should not need more input to determine position

instance (ListLike text char, Monad m) => Class.FillBuffer1 (StateT (DocumentMemory text char m) m) where
    fillBuffer1 = runCursorState Class.fillBuffer1

instance (ListLike text char, Monad m) => Class.BufferMore (StateT (DocumentMemory text char m) m) where
    bufferMore = runCursorState Class.bufferMore

instance Monad m => Class.Counting (StateT (DocumentMemory text char m) m) where
    cursorPosition = zoom cursorLens Class.cursorPosition


-- Optics

cursorLens :: Lens
    (DocumentMemory text1 char1 m1)
    (DocumentMemory text2 char2 m2)
    (Cursor (BufferedStream (StateT LineHistory m1) text1 char1))
    (Cursor (BufferedStream (StateT LineHistory m2) text2 char2))
cursorLens = lens cursor \x y -> x{ cursor = y }


-- Internal

runCursorState :: Monad m =>
    StateT (Cursor (BufferedStream (StateT LineHistory m) text char)) (StateT LineHistory m) a ->
    StateT (DocumentMemory text char m) m a
runCursorState go = do
    dm <- get
    let co = content dm
    let cu = cursor dm
    ((x, cu'), co') <- lift $ runStateT (runStateT go cu) co
    put DocumentMemory{ cursor = cu', content = co' }
    return x


-- Construction

fromListT :: Char char => ListLike text char => Monad m => ListT m text -> DocumentMemory text char m
fromListT xs =
  DocumentMemory
    { content = Lines.empty
    , cursor = Cursor.start $ BufferedStream.fromListT $ recordStream (execState . Lines.record) xs
    }


-- Cursor location

data CursorLocation =
    CursorAt Loc
  | CursorLocationNeedsMoreInput

position :: DocumentMemory text char m -> CursorLocation
position x = case Lines.locateCursorInDocument (Cursor.position (cursor x)) (content x) of
    Just l -> case l of
        Lines.CursorLocationNeedsMoreInput{ Lines.ifEndOfInput = i } ->
            if BufferedStream.isAllBuffered (Cursor.pending (cursor x)) then CursorAt i else CursorLocationNeedsMoreInput
        Lines.CursorAt l' -> CursorAt l'
    Nothing -> error "invalid DocumentMemory"
