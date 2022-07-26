{-# language FlexibleContexts, FlexibleInstances, FunctionalDependencies #-}

module Step.Document.Memory
  (
    curse,
    {- * The type -} DocumentMemory,
    {- * Construction -} fromStream,
    {- * Cursor location -} position,
  )
  where

import Step.Internal.Prelude

import Step.Document.Lines (LineHistory)
import qualified Step.Document.Lines as Lines

import Step.Input.Counter (Counter, Counting, cursorPosition)
import qualified Step.Input.Counter as Counter

import Step.Input.BufferedStream (BufferedStream)
import qualified Step.Input.BufferedStream as BufferedStream

import Step.Cursor (Cursor (..), Stream)
import qualified Step.Cursor as Cursor

import Step.Document.Locating (Locating)
import qualified Step.Document.Locating as Locating

import Step.Input.BufferedStream (LoadingDoubleBufferState)

import Step.Input.CursorPosition (CursorPosition)


-- The type

data DocumentMemory xs x m =
  DocumentMemory
    { content :: LineHistory
    , cursor :: Counter (BufferedStream (StateT LineHistory m) xs x)
    }


curse :: (ListLike xs x, Monad m) => Cursor xs x
  (StateT (DocumentMemory xs x m) m)
  (StateT CursorPosition
    (StateT (LoadingDoubleBufferState
      (StateT LineHistory m) xs x)
        (StateT LineHistory m)))
curse = Cursor.rebaseCursor runCursorState (Counter.curse BufferedStream.curse)

instance (ListLike text char, Monad m) => Locating (StateT (DocumentMemory text char m) m) where
    position = attempt1
      where
        attempt1 = use (to position) >>= \case
            Lines.CursorAt x -> return x
            Lines.CursorLocationNeedsMoreInput -> do
                runCursorState (zoom Counter.pendingLens BufferedStream.bufferMore)
                attempt2
        attempt2 = use (to position) <&> \case
            Lines.CursorAt x -> x
            Lines.CursorLocationNeedsMoreInput -> error "position @DocumentMemory" -- after buffering more, should not need more input to determine position

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

fromStream :: Lines.Char x => ListLike xs x => Monad m => Stream m xs x -> DocumentMemory xs x m
fromStream xs =
  DocumentMemory
    { content = Lines.empty
    , cursor = Counter.start $ BufferedStream.fromStream $ Cursor.record Lines.recordNontrivial xs
    }


-- Cursor location

position :: DocumentMemory text char m -> Lines.CursorLocation
position x =
    case Lines.locateCursorInDocument p ls of
        Just cl -> cl
        Nothing -> error $ "invalid DocumentMemory, lh = " <> show ls <> " does not contain position " <> show p

  where
    ls = content x
    p = Counter.position (cursor x)
