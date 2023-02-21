module Block.Sequence.Type
  (
    {- * Type -} Seq1,
  )
  where

import Essentials
import Block.Class

import Block.Null.Type (NotNull)
import Data.Sequence (Seq)

newtype Seq1 a = Seq1 (NotNull a (Seq a))
  deriving newtype
    (
      Eq, Ord, Show, Semigroup, Singleton a, Positional a,
      Search a, NonEmptyIso a, Refined a (Seq a)
    )
