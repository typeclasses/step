module Block.Class.Sequence.Type
  (
    {- * Type -} Seq1,
  )
  where

import Essentials
import Block.Class

import Block.Null.Type (NotNull)
import Data.Sequence (Seq)

import qualified Block.Class as Block
import qualified Data.Sequence as Seq

newtype Seq1 a = Seq1 (NotNull a (Seq a))
  deriving newtype
    (
      Eq, Ord, Show, Semigroup, Singleton a, Positional a,
      Search a, NonEmptyIso a, Refined a (Seq a)
    )

-- pattern (:<|) :: a -> Seq a -> Seq1 a
-- pattern x :<| xs <- (generalize -> (x Seq.:<| xs))
--   where x :<| xs = assume (x Seq.:<| xs)

-- pattern (:|>) :: Seq a -> a -> Seq1 a
-- pattern xs :|> x <- (generalize -> (xs Seq.:|> x))
--   where xs :|> x = assume (xs Seq.:|> x)

-- {-# complete (:<|) #-}

-- {-# complete (:|>) #-}
