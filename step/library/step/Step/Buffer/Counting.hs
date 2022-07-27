{-# language GeneralizedNewtypeDeriving #-}

module Step.Buffer.Counting where

import Step.Internal.Prelude

import Step.Input.CursorPosition (CursorPosition)

import Step.Cursor (Cursory (..))
import qualified Step.Cursor as Cursor
import qualified Step.Input.CursorPosition as CursorPosition

newtype Counting xs x m a = Counting (StateT CursorPosition m a)
    deriving newtype (Functor, Applicative, Monad, MonadState CursorPosition, MonadTrans)

instance (Monad m, Cursory m, CursoryText m ~ xs, CursoryChar m ~ x) => Cursory (Counting xs x m) where
    type CursoryText (Counting xs x m) = xs
    type CursoryChar (Counting xs x m) = x
    type CursoryContext (Counting xs x m) = StateT CursorPosition (CursoryContext m)

    cursoryRun a =
        get >>= \p -> lift (cursoryRun @m (runStateT a p)) >>= \(x, p') -> put p' $> x

    cursoryInput =
        Cursor.rebaseStream lift $ cursoryInput @m

    cursoryCommit n =
        modify' (CursorPosition.strictlyIncrease n) *> lift (cursoryCommit @m n)
