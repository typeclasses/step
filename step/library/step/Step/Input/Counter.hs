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

import qualified ListLike

import Step.Input.Cursor (Cursor (..))

import qualified Step.Input.AdvanceResult as Advance

import qualified Positive

import Step.Input.Buffering (Buffering (..))

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

instance (Monad m, Cursor (StateT input m)) => Cursor (StateT (Counter input) m) where
    type Text (StateT (Counter input) m) = Text (StateT input m)
    type Char (StateT (Counter input) m) = Char (StateT input m)
    forecast = changeBaseListT (zoom pendingLens) forecast
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

instance (Monad m, Buffering (StateT input m)) =>
    Buffering (StateT (Counter input) m)
  where
    fillBuffer1 = zoom pendingLens fillBuffer1
    bufferMore = zoom pendingLens bufferMore

---

positionLens :: Lens' (Counter input) CursorPosition
positionLens = lens position \x y -> x{ position = y }

pendingLens :: Lens' (Counter input) input
pendingLens = lens pending \x y -> x{ pending = y }

---

start :: input -> Counter input
start = Counter 0
