module Block.BlockSequence.Type
  (
    BlockSequence,
  )
  where

import Essentials

import Block.Sequence.Type (Seq1 (..))
import Block.Class.Block (Block, Item, Pop (..))
import Integer (Positive)
import Prelude ((+), (-))
import Data.Sequence (Seq)
import Data.List.NonEmpty (NonEmpty ((:|)))

import qualified Data.Foldable as Foldable
import qualified Block.Class as Block

data BlockSequence block = BlockSequence{ seq1 :: !(Seq1 block), length :: !Positive }

instance Semigroup (BlockSequence block) where
    BlockSequence a a' <> BlockSequence b b' = BlockSequence (a <> b) (a' + b')

type instance Block.Item (BlockSequence block) = Block.Item block

instance Block block => Block (BlockSequence block) where

    length :: BlockSequence block -> Positive
    length = length

    concat :: NonEmpty (BlockSequence block) -> BlockSequence block
    concat (x :| xs) = Foldable.foldl' (<>) x xs

    leftView (BlockSequence (xs :<| xss) len) = Block.Pop{ popItem, popRemainder }
      where
        popItem :: Item block
        headRemainder :: Maybe block
        popRemainder :: Maybe (BlockSequence block)

        Block.Pop popItem headRemainder = Block.leftView xs

        popRemainder = case headRemainder of
            Nothing -> Block.refine xss <&> mkRemainder
            Just xs' -> Just $ mkRemainder (xs' :<| xss)
          where
            mkRemainder :: Seq1 block -> BlockSequence block
            mkRemainder z = BlockSequence z (len - 1)

    -- span p = fmap coerce . span @(LL1 (Seq a)) p . coerce

    -- split n = fmap coerce . split @(LL1 (Seq a)) n . coerce

    -- take n = fmap coerce . take @(LL1 (Seq a)) n . coerce

    -- drop n = fmap coerce . drop @(LL1 (Seq a)) n . coerce

    -- while p = fmap coerce . while @(LL1 (Seq a)) p . coerce
