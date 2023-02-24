module Block.Class.Positional.Types
  (
    {- * Types -} Take (..), BiPrefix (..), WhichOfTwo (..),
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

data WhichOfTwo = First | Second
    deriving stock (Eq, Ord, Show, Enum, Bounded)

data BiPrefix xs =
    BothPrefix -- ^ The two blocks are equivalent
  | NoPrefixRelation -- ^ Neither block is a prefix of the other
  | IsPrefix
      -- ^ The ('First' or 'Second') block is a prefix of the other
      WhichOfTwo xs xs
    deriving stock (Eq, Ord, Show, Functor)
