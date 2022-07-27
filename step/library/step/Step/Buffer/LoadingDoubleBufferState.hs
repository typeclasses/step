{-# language DerivingVia #-}

module Step.Buffer.LoadingDoubleBufferState (LoadingDoubleBufferState (..)) where

import Step.Internal.Prelude

import Step.Cursor (Stream)
import Step.Buffer.DoubleBufferState (DoubleBufferState)

newtype LoadingDoubleBufferState xs x m a =
    LoadingDoubleBufferState (Stream m xs x -> DoubleBufferState xs x m a)
    deriving (Functor, Applicative, Monad)
        via ReaderT (Stream m xs x) (DoubleBufferState xs x m)

instance MonadTrans (LoadingDoubleBufferState xs x) where
    lift a = LoadingDoubleBufferState \_ -> lift a
