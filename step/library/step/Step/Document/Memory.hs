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

import Step.Input.Counter (Counter)
import qualified Step.Input.Counter as Counter

import Step.Input.BufferedStream (BufferedStream)
import qualified Step.Input.BufferedStream as BufferedStream

import Step.Input.CursorPosition (CursorPosition)

import Loc (Loc)

import Step.Input.Cursor (Cursor (..))

import qualified Step.Input.AdvanceResult as Advance

import Step.Input.Counter (Counting, cursorPosition)
import qualified Step.Input.Counter

import Step.Input.Buffering (Buffering (..))

import Step.Document.Locating (Locating)
import qualified Step.Document.Locating as Locating


-- The type

data DocumentMemory text char m =
  DocumentMemory
    { content :: LineHistory
    , cursor :: Counter (BufferedStream (StateT LineHistory m) text char)
    }

instance (ListLike text char, Monad m) => Cursor (StateT (DocumentMemory text char m) m) where
    type Text (StateT (DocumentMemory text char m) m) = text
    type Char (StateT (DocumentMemory text char m) m) = char
    forecast = changeBaseListT runCursorState forecast
    advance n = runCursorState (advance n)

instance (ListLike text char, Monad m) => Locating (StateT (DocumentMemory text char m) m) where
    position = attempt1
      where
        attempt1 = use (to position) >>= \case
            CursorAt x -> return x
            CursorLocationNeedsMoreInput -> bufferMore *> attempt2
        attempt2 = use (to position) <&> \case
            CursorAt x -> x
            CursorLocationNeedsMoreInput -> error "position @DocumentMemory" -- after buffering more, should not need more input to determine position

instance (ListLike text char, Monad m) => Buffering (StateT (DocumentMemory text char m) m) where
    fillBuffer1 = runCursorState fillBuffer1
    bufferMore = runCursorState bufferMore

instance Monad m => Counting (StateT (DocumentMemory text char m) m) where
    cursorPosition = zoom cursorLens cursorPosition


-- Optics

cursorLens :: Lens
    (DocumentMemory text1 char1 m1)
    (DocumentMemory text2 char2 m2)
    (Counter (BufferedStream (StateT LineHistory m1) text1 char1))
    (Counter (BufferedStream (StateT LineHistory m2) text2 char2))
cursorLens = lens cursor \x y -> x{ cursor = y }


-- Internal

runCursorState :: Monad m =>
    StateT (Counter (BufferedStream (StateT LineHistory m) text char)) (StateT LineHistory m) a ->
    StateT (DocumentMemory text char m) m a
runCursorState go = do
    dm <- get
    let co = content dm
    let cu = cursor dm
    ((x, cu'), co') <- lift $ runStateT (runStateT go cu) co
    put DocumentMemory{ cursor = cu', content = co' }
    return x


-- Construction

fromListT :: Lines.Char char => ListLike text char => Monad m => ListT m text -> DocumentMemory text char m
fromListT xs =
  DocumentMemory
    { content = Lines.empty
    , cursor = Counter.start $ BufferedStream.fromListT $ recordStream (execState . Lines.record) xs
    }


-- Cursor location

data CursorLocation =
    CursorAt Loc
  | CursorLocationNeedsMoreInput

position :: DocumentMemory text char m -> CursorLocation
position x = case Lines.locateCursorInDocument (Counter.position (cursor x)) (content x) of
    Just l -> case l of
        Lines.CursorLocationNeedsMoreInput{ Lines.ifEndOfInput = i } ->
            if BufferedStream.isAllBuffered (Counter.pending (cursor x)) then CursorAt i else CursorLocationNeedsMoreInput
        Lines.CursorAt l' -> CursorAt l'
    Nothing -> error "invalid DocumentMemory"
