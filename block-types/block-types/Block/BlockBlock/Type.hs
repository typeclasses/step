{-# language UndecidableInstances #-}

module Block.BlockBlock.Type
  (
    BlockBlock (..),
  )
  where

import Essentials

import Block.Sequence.Type (Seq1 (..))
import Block.Class.Block (Block, Item, Pop (..))
import Integer (Positive)
import Prelude ((+), (-))
import Data.List.NonEmpty (NonEmpty ((:|)))

import qualified Data.Foldable as Foldable
import qualified Block.Class as Block

newtype BlockBlock xss = BlockBlock{ blockBlock :: xss }
    deriving newtype Semigroup

type instance Item (BlockBlock xss) = Item (Item xss)

instance (Block xss, Block xs, Item xss ~ xs, Item xs ~ x) => Block (BlockBlock xss) where

    length :: BlockBlock xss -> Positive
    length = blockBlock >>> Block.length

    concat :: NonEmpty (BlockBlock xss) -> BlockBlock xss
    concat (x :| xs) = Foldable.foldl' (<>) x xs

    leftView :: BlockBlock xss -> Pop (BlockBlock xss)
    leftView
      BlockBlock
        { blockBlock = Block.leftView ->
            Pop
              { popItem = Block.leftView ->
                  Pop{ popItem = x, popRemainder = xsMaybe }
              , popRemainder = xssMaybe
              }
        } =
        Pop
          { popItem = x
          , popRemainder = BlockBlock <$> case xsMaybe of
              Nothing -> xssMaybe
              Just xs' -> Just $ Block.leftReview $ Pop xs' xssMaybe
          }

    -- span p = fmap coerce . span @(LL1 (Seq a)) p . coerce

    -- split n = fmap coerce . split @(LL1 (Seq a)) n . coerce

    -- take n = fmap coerce . take @(LL1 (Seq a)) n . coerce

    -- drop n = fmap coerce . drop @(LL1 (Seq a)) n . coerce

    -- while p = fmap coerce . while @(LL1 (Seq a)) p . coerce
