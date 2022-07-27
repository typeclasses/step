{-# language DerivingVia, GeneralizedNewtypeDeriving #-}

module Step.Buffer.DoubleBufferState
  (
    DoubleBufferState (..),
  )
  where

import Step.Internal.Prelude hiding (fold)

import Step.Buffer.Buffer (Buffer)
import qualified Step.Buffer.Buffer as Buffer

import Step.Cursor (AdvanceResult (..), Cursor (..), Stream)
import qualified Step.Cursor as Cursor

import Step.Buffer.BufferResult (BufferResult(..))

import Step.Buffer.DoubleBuffer (DoubleBuffer (DoubleBuffer), unseenLens, uncommittedLens)
import qualified Step.Buffer.DoubleBuffer as DoubleBuffer

newtype DoubleBufferState xs x m a =
    DoubleBufferState (StateT (DoubleBuffer xs x) m a)
    deriving newtype (Functor, Applicative, Monad)
