{-# language FlexibleContexts, FlexibleInstances, TypeFamilies #-}

module Step.Input.Cursor
  (
    Cursor (..),
    start,
    -- {- * Conversion with ListT -} fromListT, toListT,
  )
  where

import Step.Internal.Prelude

import Step.Input.BufferedStream (BufferedStream)

import Step.Input.CursorPosition (CursorPosition)
import qualified Step.Input.CursorPosition as CursorPosition

import Step.Nontrivial.Base (Nontrivial)

import Step.TakeOrLeave (TakeOrLeave (..))

import qualified Step.Classes.Base as Class

import qualified ListLike

---

data Cursor m text char =
  Cursor
    { position :: CursorPosition
    , pending :: BufferedStream m text char
    }

instance (ListLike text char, Monad m) =>
    Class.Char1 (StateT (Cursor m text char) m)
  where
    type Text (StateT (Cursor m text char) m) = text
    type Char (StateT (Cursor m text char) m) = char
    peekCharMaybe = zoom pendingLens Class.peekCharMaybe
    atEnd = zoom pendingLens Class.atEnd
    considerChar c = do
        r <- zoom pendingLens (Class.considerChar c)
        case r of{ Just (Take _) -> modifying positionLens (CursorPosition.increase 1); _ -> return () }
        return r

instance (ListLike text char, Monad m) =>
    Class.TakeAll (StateT (Cursor m text char) m)
  where
    takeAll = do
        x <- zoom pendingLens Class.takeAll
        modifying positionLens (+ fromIntegral (ListLike.length x))
        return x

instance (ListLike text char, Monad m, Eq text, Eq char) =>
    Class.SkipTextNonAtomic (StateT (Cursor m text char) m)
  where
    skipTextNonAtomic x = do
        y <- zoom pendingLens (Class.skipTextNonAtomic x)
        modifying positionLens (+ fromIntegral (ListLike.length x))
        return y

instance (ListLike text char, Monad m) =>
    Class.FillBuffer1 (StateT (Cursor m text char) m)
  where
    fillBuffer1 = zoom pendingLens Class.fillBuffer1

instance (ListLike text char, Monad m) =>
    Class.BufferMore (StateT (Cursor m text char) m)
  where
    bufferMore = zoom pendingLens Class.bufferMore

---

positionLens :: Lens' (Cursor m text char) CursorPosition
positionLens = lens position \x y -> x{ position = y }

pendingLens :: Lens' (Cursor m text char) (BufferedStream m text char)
pendingLens = lens pending \x y -> x{ pending = y }

---

start :: Monad m => ListLike text char => BufferedStream m text char -> Cursor m text char
start = Cursor 0
