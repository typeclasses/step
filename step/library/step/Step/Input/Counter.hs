{-# language FlexibleContexts, FlexibleInstances, FunctionalDependencies, TypeFamilies #-}

module Step.Input.Counter
  (
    Counting (..),
    Counter (..),
    start,
  )
  where

import Step.Internal.Prelude

import Step.Input.CursorPosition (CursorPosition)
import qualified Step.Input.CursorPosition as CursorPosition

import Step.Nontrivial.Base (Nontrivial)

import qualified Step.Classes.Base as Class

import qualified ListLike

import Step.Advancement (AdvanceResult, Progressive (..))
import qualified Step.Advancement as Advance

import Step.LookingAhead (Prophetic (..))

import qualified Positive

---

class Monad m => Counting m where
    cursorPosition :: m CursorPosition

instance (Monad m, Counting m) => Counting (ReaderT r m) where
    cursorPosition = lift cursorPosition

---

data Counter input =
  Counter
    { position :: CursorPosition
    , pending :: input
    }

instance (Monad m, Prophetic (StateT input m)) => Prophetic (StateT (Counter input) m) where
    type Text (StateT (Counter input) m) = Text (StateT input m)
    type Char (StateT (Counter input) m) = Char (StateT input m)
    forecast = changeBaseListT (zoom pendingLens) forecast

instance (Monad m, Progressive (StateT input m)) => Progressive (StateT (Counter input) m) where
    advance n = do
        r <- zoom pendingLens (advance n)
        let delta = case r of
                Advance.Success -> review Positive.refine n
                Advance.InsufficientInput{ Advance.shortfall = s } -> review Positive.refine n - review Positive.refine s
        modifying positionLens (CursorPosition.increase delta)
        return r

instance Monad m => Counting (StateT (Counter input) m)
  where
    cursorPosition = get <&> position

instance (Monad m, Class.FillBuffer1 (StateT input m)) =>
    Class.FillBuffer1 (StateT (Counter input) m)
  where
    fillBuffer1 = zoom pendingLens Class.fillBuffer1

instance (Monad m, Class.BufferMore (StateT input m)) =>
    Class.BufferMore (StateT (Counter input) m)
  where
    bufferMore = zoom pendingLens Class.bufferMore

---

positionLens :: Lens' (Counter input) CursorPosition
positionLens = lens position \x y -> x{ position = y }

pendingLens :: Lens' (Counter input) input
pendingLens = lens pending \x y -> x{ pending = y }

---

start :: input -> Counter input
start = Counter 0
