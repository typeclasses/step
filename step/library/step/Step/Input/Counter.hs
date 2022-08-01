{-# language FlexibleContexts, FlexibleInstances, FunctionalDependencies #-}

module Step.Input.Counter
  (
    KnowsCursorPosition (..),
  )
  where

import Step.Internal.Prelude

import Step.Input.CursorPosition (CursorPosition)
import qualified Step.Input.CursorPosition as CursorPosition

import Step.Cursor (ReadWriteCursor (..), AdvanceResult (..), Stream)
import qualified Step.Cursor as Cursor

import qualified Positive

---

class Monad m => KnowsCursorPosition m where
    cursorPosition :: m CursorPosition

instance (Monad m, KnowsCursorPosition m) => KnowsCursorPosition (ReaderT r m) where
    cursorPosition = lift cursorPosition
