{-# language FlexibleContexts, FlexibleInstances, FunctionalDependencies, TypeFamilies #-}

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

import Step.Advancement (AdvanceResult, Progressive (..))
import qualified Step.Advancement as Advance

import Step.LookingAhead (Prophetic (..))

import qualified Positive

---

data Cursor input =
  Cursor
    { position :: CursorPosition
    , pending :: input
    }

instance (Monad m, Prophetic (StateT input m)) => Prophetic (StateT (Cursor input) m) where
    type Text (StateT (Cursor input) m) = Text (StateT input m)
    type Char (StateT (Cursor input) m) = Char (StateT input m)
    forecast = changeBaseListT (zoom pendingLens) forecast

instance (Monad m, Progressive (StateT input m)) => Progressive (StateT (Cursor input) m) where
    advance n = do
        r <- zoom pendingLens (advance n)
        let delta = case r of
                Advance.Success -> review Positive.refine n
                Advance.InsufficientInput{ Advance.shortfall = s } -> review Positive.refine n - review Positive.refine s
        modifying positionLens (CursorPosition.increase delta)
        return r

instance Monad m =>
    Class.Counting (StateT (Cursor input) m)
  where
    cursorPosition = get <&> position

instance (Monad m, Class.Char1 (StateT input m)) =>
    Class.Char1 (StateT (Cursor input) m)
  where
    type Text (StateT (Cursor input) m) = Class.Text (StateT input m)
    type Char (StateT (Cursor input) m) = Class.Char (StateT input m)

instance (Monad m, Class.FillBuffer1 (StateT input m)) =>
    Class.FillBuffer1 (StateT (Cursor input) m)
  where
    fillBuffer1 = zoom pendingLens Class.fillBuffer1

instance (Monad m, Class.BufferMore (StateT input m)) =>
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
