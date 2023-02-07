module Block.Sequence.Internal,
  (
    {- * Type -} Seq1 (Seq1, (:<|), (:|>)),
  )
  where

import Essentials
import Block.Class

import Block.ListLike (LL1)
import Data.Coerce (coerce)
import Data.Sequence (Seq)

import qualified Block.Class as Block
import qualified Data.Sequence as Seq

newtype Seq1 a = Seq1 (LL1 (Seq a))
    deriving stock (Eq, Ord, Show)

instance Semigroup (Seq1 a) where
    Seq1 a <> Seq1 b = Seq1 (a <> b)

instance Trivializable (Seq1 a) where
    refine = fmap Seq1 . refine
    generalize (Seq1 x) = generalize x
    assume = Seq1 . assume

type instance Block.Item (Seq1 a) = a

type instance Nullable (Seq1 a) = Seq a

instance Block (Seq1 a) where

    -- All just coercions of LL1 methods
    singleton = coerce . singleton @(LL1 (Seq a))
    concat = coerce . concat @(LL1 (Seq a)) . coerce
    leftView = (\(Pop i r) -> Pop i (coerce r)) . leftView @(LL1 (Seq a)) . coerce
    leftReview = coerce . leftReview @(LL1 (Seq a)) . (\(Pop i r) -> Pop i (coerce r))
    span p = fmap coerce . span @(LL1 (Seq a)) p . coerce
    divide f = fmap coerce . divide @(LL1 (Seq a)) f . coerce
    split n = fmap coerce . split @(LL1 (Seq a)) n . coerce
    take n = fmap coerce . take @(LL1 (Seq a)) n . coerce
    drop n = fmap coerce . drop @(LL1 (Seq a)) n . coerce
    while p = fmap coerce . while @(LL1 (Seq a)) p . coerce
    length = length @(LL1 (Seq a)) . coerce

pattern (:<|) :: a -> Seq a -> Seq1 a
pattern x :<| xs <- (generalize -> (x Seq.:<| xs))
  where x :<| xs = assume (x Seq.:<| xs)

pattern (:|>) :: Seq a -> a -> Seq1 a
pattern xs :|> x <- (generalize -> (xs Seq.:|> x))
  where xs :|> x = assume (xs Seq.:|> x)

{-# complete (:<|) #-}

{-# complete (:|>) #-}
