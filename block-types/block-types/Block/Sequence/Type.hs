module Block.Sequence.Type
  (
    {- * Type -} Seq1 (..),
  )
  where

import Essentials
import Block.Class

import Block.ListLike (LL1)
import Data.Coerce (coerce)
import Data.Sequence (Seq)

import qualified Block.Class as Block

newtype Seq1 a = Seq1 (LL1 (Seq a))
    deriving stock (Eq, Ord, Show)

instance Semigroup (Seq1 a) where
    Seq1 a <> Seq1 b = Seq1 (a <> b)

instance Trivializable (Seq1 a) where
    refine = fmap Seq1 . refine
    generalize (Seq1 x) = generalize x

type instance Block.Item (Seq1 a) = a

type instance Nullable (Seq1 a) = Seq a

instance Block (Seq1 a) where

    -- All just coercions of LL1 methods
    concat = coerce . concat @(LL1 (Seq a)) . coerce
    leftView = (\(Pop i r) -> Pop i (coerce r)) . leftView @(LL1 (Seq a)) . coerce
    span p = fmap coerce . span @(LL1 (Seq a)) p . coerce
    split n = fmap coerce . split @(LL1 (Seq a)) n . coerce
    take n = fmap coerce . take @(LL1 (Seq a)) n . coerce
    drop n = fmap coerce . drop @(LL1 (Seq a)) n . coerce
    while p = fmap coerce . while @(LL1 (Seq a)) p . coerce
    length = length @(LL1 (Seq a)) . coerce
