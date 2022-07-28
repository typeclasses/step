{-# language FlexibleContexts, GeneralizedNewtypeDeriving #-}

module Step.Buffer.Counting where

import Step.Internal.Prelude

import Step.Input.CursorPosition (CursorPosition)

import Step.Cursor (Cursor (Cursor), Stream, AdvanceResult)
import qualified Step.Cursor as Cursor
import qualified Step.Input.CursorPosition as CursorPosition

newtype Counting xs x m a = Counting (StateT CursorPosition m a)
    deriving newtype (Functor, Applicative, Monad, MonadState CursorPosition, MonadTrans)

countingCursor :: forall m m' xs x. (Monad m, Monad m') =>
    Cursor xs x m m' -> Cursor xs x (Counting xs x m) (StateT CursorPosition m')
countingCursor c = Cursor{ Cursor.run, Cursor.input, Cursor.commit }
  where
    run :: StateT CursorPosition m' a -> Counting xs x m a
    run a = get >>= \p -> lift (Cursor.run c (runStateT a p)) >>= \(x, p') -> put p' $> x

    input :: Stream (StateT CursorPosition m') xs x
    input = Cursor.rebaseStream lift $ Cursor.input c

    commit :: Positive Natural -> StateT CursorPosition m' AdvanceResult
    commit n = modify' (CursorPosition.strictlyIncrease n) *> lift (Cursor.commit c n)
