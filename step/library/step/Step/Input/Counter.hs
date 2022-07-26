{-# language FlexibleContexts, FlexibleInstances, FunctionalDependencies #-}

module Step.Input.Counter
  (
    Counting (..),
    Counter (..),
    start, curse,
    positionLens, pendingLens,
  )
  where

import Step.Internal.Prelude

import Step.Input.CursorPosition (CursorPosition)
import qualified Step.Input.CursorPosition as CursorPosition

import Step.Cursor (Cursor (..), AdvanceResult (..), Stream)
import qualified Step.Cursor as Cursor

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

curse :: forall m input xs x. Monad m =>
    Cursor xs x (StateT input m) -> Cursor xs x (StateT (Counter input) m)
curse Cursor{ run = (runUpstream :: forall a. m' a -> StateT input m a), input = inputUpstream, commit = commitUpstream } =
    Cursor{ run, input, commit }
  where
    run :: StateT CursorPosition m' a -> StateT (Counter input) m a
    run a = do
        p <- get <&> position
        (x, p') <- zoom pendingLens (runUpstream (runStateT a p))
        assign positionLens p'
        return x

    input :: Stream (StateT CursorPosition m') xs x
    input = Cursor.rebaseStream lift inputUpstream

    commit :: Positive Natural -> StateT CursorPosition m' AdvanceResult
    commit n = do
        r <- lift (commitUpstream n)
        modify' $ CursorPosition.increase $ case r of
            AdvanceSuccess -> review Positive.refine n
            YouCanNotAdvance{ shortfall = s } -> review Positive.refine n - review Positive.refine s
        return r

instance Monad m => Counting (StateT (Counter input) m)
  where
    cursorPosition = get <&> position

---

positionLens :: Lens' (Counter input) CursorPosition
positionLens = lens position \x y -> x{ position = y }

pendingLens :: Lens' (Counter input) input
pendingLens = lens pending \x y -> x{ pending = y }

---

start :: input -> Counter input
start = Counter 0
