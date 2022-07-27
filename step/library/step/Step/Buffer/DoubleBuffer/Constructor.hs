module Step.Buffer.DoubleBuffer.Constructor (DoubleBuffer (..)) where

import Step.Internal.Prelude

import Step.Buffer.Buffer (Buffer)

data DoubleBuffer xs x =
  DoubleBuffer
    { unseen :: Buffer xs x
    , uncommitted :: Buffer xs x
    }
    deriving stock (Eq, Show)
