{-# language FlexibleContexts, FlexibleInstances, TypeFamilies #-}

module Step.Input.Cursor
  (
    Cursor (..),
    start,
  )
  where

import Step.Internal.Prelude

import Step.Input.CursorPosition (CursorPosition)
import qualified Step.Input.CursorPosition as CursorPosition

import Step.Nontrivial.Base (Nontrivial)

import Step.TakeOrLeave (TakeOrLeave (..))

import qualified Step.Classes.Base as Class

import qualified ListLike

---

data Cursor input =
  Cursor
    { position :: CursorPosition
    , pending :: input
    }

instance
    ( Monad m
    , Class.Char1 (StateT input m)
    ) =>
    Class.Char1 (StateT (Cursor input) m)
  where
    type Text (StateT (Cursor input) m) = Class.Text (StateT input m)
    type Char (StateT (Cursor input) m) = Class.Char (StateT input m)
    peekCharMaybe = zoom pendingLens Class.peekCharMaybe
    atEnd = zoom pendingLens Class.atEnd
    considerChar c = do
        r <- zoom pendingLens (Class.considerChar c)
        case r of{ Just (Take _) -> modifying positionLens (CursorPosition.increase 1); _ -> return () }
        return r

instance
    ( Monad m
    , Class.TakeAll (StateT input m)
    ) =>
    Class.TakeAll (StateT (Cursor input) m)
  where
    takeAll = do
        x <- zoom pendingLens Class.takeAll
        modifying positionLens (+ fromIntegral (ListLike.length x))
        return x

instance
    ( Monad m
    , Class.SkipTextNonAtomic (StateT input m)
    ) =>
    Class.SkipTextNonAtomic (StateT (Cursor input) m)
  where
    skipTextNonAtomic x = do
        y <- zoom pendingLens (Class.skipTextNonAtomic x)
        modifying positionLens (+ fromIntegral (ListLike.length x))
        return y

instance
    ( Monad m
    , Class.FillBuffer1 (StateT input m)
    ) =>
    Class.FillBuffer1 (StateT (Cursor input) m)
  where
    fillBuffer1 = zoom pendingLens Class.fillBuffer1

instance
    ( Monad m
    , Class.BufferMore (StateT input m)
    ) =>
    Class.BufferMore (StateT (Cursor input) m)
  where
    bufferMore = zoom pendingLens Class.bufferMore

---

positionLens :: Lens' (Cursor input) CursorPosition
positionLens = lens position \x y -> x{ position = y }

pendingLens :: Lens' (Cursor input) input
pendingLens = lens pending \x y -> x{ pending = y }

---

start :: input -> Cursor input
start = Cursor 0
