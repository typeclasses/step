{-# language FlexibleContexts, FlexibleInstances, FunctionalDependencies, RankNTypes #-}

module Step.Input.Counter
  (
    Counting (..),
    Counter (..),
    start,
    curse,
  )
  where

import Step.Internal.Prelude

import Step.Input.CursorPosition (CursorPosition)
import qualified Step.Input.CursorPosition as CursorPosition

import Step.Nontrivial.Base (Nontrivial)

import Step.Input.Cursor (Session (..))
import qualified Step.Input.Cursor as Cursor

import qualified Step.Input.AdvanceResult as Advance
import Step.Input.AdvanceResult (AdvanceResult)

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

curse :: forall m input text char. Monad m =>
    Session text char (StateT input m)
    -> Session text char (StateT (Counter input) m)
curse Session{ run = (run' :: forall a. m' a -> StateT input m a), next = next', commit = commit' } =
    Cursor.Session{ run, next, commit }
  where
    run :: StateT CursorPosition m' a -> StateT (Counter input) m a
    run a = do
        p <- get <&> position
        (x, p') <- zoom pendingLens (run' (runStateT a p))
        assign positionLens p'
        return x

    next :: StateT CursorPosition m' (Maybe (Nontrivial text char))
    next = lift next'

    commit :: Positive Natural -> StateT CursorPosition m' AdvanceResult
    commit n = do
        r <- lift (commit' n)
        modify' $ CursorPosition.increase $ case r of
            Advance.Success -> review Positive.refine n
            Advance.InsufficientInput{ Advance.shortfall = s } -> review Positive.refine n - review Positive.refine s
        return r

instance Monad m => Counting (StateT (Counter input) m)
  where
    cursorPosition = get <&> position

instance (Monad m, Buffering (StateT input m)) =>
    Buffering (StateT (Counter input) m)
  where
    bufferMore = zoom pendingLens bufferMore

---

positionLens :: Lens' (Counter input) CursorPosition
positionLens = lens position \x y -> x{ position = y }

pendingLens :: Lens' (Counter input) input
pendingLens = lens pending \x y -> x{ pending = y }

---

start :: input -> Counter input
start = Counter 0
