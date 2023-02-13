module Block.Class.BiPrefix.Types
  (
    BiPrefix (..),
    WhichOfTwo (..),
  )
  where

import Essentials

data WhichOfTwo = First | Second
    deriving stock (Eq, Ord, Show, Enum, Bounded)

data BiPrefix xs =
    Same -- ^ The two blocks are equivalent
  | NoPrefixRelation -- ^ Neither block is a prefix of the other
  | IsPrefix
      -- ^ The ('First' or 'Second') block is a prefix of the other
      WhichOfTwo xs xs
    deriving stock (Eq, Ord, Show, Functor)
