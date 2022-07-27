{-# language FlexibleContexts, DerivingVia, GeneralizedNewtypeDeriving #-}

module Step.Buffer.LoadingDoubleBufferState
  (
    LoadingDoubleBufferState (..),
  )
  where

import Step.Internal.Prelude hiding (fold)

import Step.Buffer.Buffer (Buffer)
import qualified Step.Buffer.Buffer as Buffer

import Step.Cursor (AdvanceResult (..), Cursor (..), Stream)
import qualified Step.Cursor as Cursor

import Step.Buffer.BufferResult (BufferResult(..))

import Step.Buffer.DoubleBuffer (DoubleBuffer (DoubleBuffer), unseenLens, uncommittedLens)

import Step.Buffer.DoubleBufferState (DoubleBufferState (..))
import qualified Step.Buffer.DoubleBufferState as DoubleBufferState

newtype LoadingDoubleBufferState xs x m a =
    LoadingDoubleBufferState (Stream m xs x -> DoubleBufferState xs x m a)
    deriving (Functor, Applicative, Monad)
        via ReaderT (Stream m xs x) (DoubleBufferState xs x m)
