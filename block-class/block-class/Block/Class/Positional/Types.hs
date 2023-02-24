module Block.Class.Positional.Types
  (
    {- * Types -} Take (..),
  )
  where

import Essentials

import Block.Class.Shortfall

{-| The result of 'Block.Class.take' -}
data Take xs =
    TakePart{ taken :: xs, takeRemainder :: xs }
      -- ^ The requested range covers part of the block
  | TakeAll
      -- ^ The requested range covers the entire block
  | TakeInsufficient Shortfall
      -- ^ The requested range exceeds the size of the block
  deriving stock (Eq, Ord, Show, Functor)
