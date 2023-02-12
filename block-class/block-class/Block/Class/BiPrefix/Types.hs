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
    Same
  | NoPrefixRelation
  | IsPrefix WhichOfTwo xs xs
    deriving stock (Eq, Ord, Show, Functor)
