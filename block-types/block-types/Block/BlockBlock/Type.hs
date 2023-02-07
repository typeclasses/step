{-# language UndecidableInstances #-}

module Block.BlockBlock.Type
  (
    BlockBlock (..),
  )
  where

import Essentials

import Block.Sequence.Type (Seq1 (..))
import Block.Class.Block (Block, Item, Pop (..), Division (..))
import Integer (Positive)
import Prelude ((+), (-))
import Data.List.NonEmpty (NonEmpty ((:|)))

import qualified Data.Foldable as Foldable
import qualified Block.Class as Block

newtype BlockBlock xss = BlockBlock{ blockBlock :: xss }
    deriving newtype Semigroup

type instance Item (BlockBlock xss) = Item (Item xss)

leftView' :: (Block xss, Block xs, Item xss ~ xs, Item xs ~ x) =>
    xss -> (x, Maybe xs, Maybe xss)
leftView' (Block.leftView -> Pop (Block.leftView -> Pop x xsMaybe) xssMaybe) =
    (x, xsMaybe, xssMaybe)

instance (Block xss, Block xs, Item xss ~ xs, Item xs ~ x) => Block (BlockBlock xss) where

    length :: BlockBlock xss -> Positive
    length = blockBlock >>> Block.length

    concat :: NonEmpty (BlockBlock xss) -> BlockBlock xss
    concat (x :| xs) = Foldable.foldl' (<>) x xs

    leftView :: BlockBlock xss -> Pop (BlockBlock xss)
    leftView (BlockBlock (leftView' -> (x, xsMaybe, xssMaybe))) =
        Pop x $ BlockBlock <$> case xsMaybe of
            Nothing -> xssMaybe
            Just xs' -> Just $ Block.leftReview $ Pop xs' xssMaybe

    divide :: forall a. (x -> Maybe a) -> BlockBlock xss -> Division a (BlockBlock xss)
    divide f (BlockBlock xss) = fmap BlockBlock $ Division $ distribute _
      where
        distribute :: (Maybe xss, (Maybe xs, a, Maybe xs), Maybe xss) -> (Maybe xss, a, Maybe xss)
        distribute (a, (a', x, b'), b) = (a <> a', x, b' <> b)

    -- span p = fmap coerce . span @(LL1 (Seq a)) p . coerce

    -- split n = fmap coerce . split @(LL1 (Seq a)) n . coerce

    -- take n = fmap coerce . take @(LL1 (Seq a)) n . coerce

    -- drop n = fmap coerce . drop @(LL1 (Seq a)) n . coerce

    -- while p = fmap coerce . while @(LL1 (Seq a)) p . coerce
