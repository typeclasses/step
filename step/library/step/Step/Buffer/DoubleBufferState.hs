{-# language GeneralizedNewtypeDeriving #-}

module Step.Buffer.DoubleBufferState (DoubleBufferState (..)) where

import Step.Internal.Prelude

import Step.Buffer.DoubleBuffer (DoubleBuffer)

newtype DoubleBufferState xs x m a =
    DoubleBufferState (StateT (DoubleBuffer xs x) m a)
    deriving newtype (Functor, Applicative, Monad, MonadTrans)
