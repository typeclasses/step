{-# language StrictData #-}

module Block.BlockBlock.Type
  (
    BlockBlock (..),
  )
  where

import Essentials
import Block.Class

import Integer (Positive)
import Prelude ((+), (-))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Semigroup (sconcat)
import Data.Ord (Ordering (..))
import Integer.Signed (Signed (..))

import qualified Data.Foldable as Foldable
import qualified Block.Class as Block
import qualified Data.Maybe as Maybe
import qualified Fold.Nonempty as Fold
import qualified Integer.Positive as Positive

data BlockBlock x xs xss = BlockBlock{ bbXss :: xss, bbLength :: Positive }
    deriving stock (Eq, Ord, Show)

bb :: forall x xs xss. (NonEmptyIso xs xss, Positional x xs) => xss -> BlockBlock x xs xss
bb xss = BlockBlock xss (Fold.run Fold.sum (length <$> (toNonEmpty Front xss :: NonEmpty xs)))

instance (Semigroup xss) => Semigroup (BlockBlock x xs xss) where
    BlockBlock xss1 len1 <> BlockBlock xss2 len2 = BlockBlock (xss1 <> xss2) (len1 + len2)

instance (NonEmptyIso x xs, NonEmptyIso xs xss, Singleton xs xss, Positional x xs) => NonEmptyIso x (BlockBlock x xs xss) where

    toNonEmpty :: End -> BlockBlock x xs xss -> NonEmpty x
    toNonEmpty end = bbXss >>> toNonEmpty end >>> fmap (toNonEmpty end) >>> sconcat

    fromNonEmpty :: End -> NonEmpty x -> BlockBlock x xs xss
    fromNonEmpty end = fromNonEmpty end >>> singleton >>> bb

instance Positional x (BlockBlock x xs xss) where

    length :: BlockBlock x xs xss -> Positive
    length = bbLength

    take :: End -> Positive -> BlockBlock x xs xss -> Take (BlockBlock x xs xss)
    take end n (BlockBlock xss len) = case Positive.subtract len n of
        Minus s -> TakeInsufficient (Shortfall s)
        Zero -> TakeAll
        Plus remainderLength -> TakePart (BlockBlock taken n) (BlockBlock remainder remainderLength)
          where
            (taken, remainder) = undefined -- todo

-- leftView' :: (Block xss, Block xs, Item xss ~ xs, Item xs ~ x) =>
--     xss -> (x, Maybe xs, Maybe xss)
-- leftView' (Block.leftView -> Pop (Block.leftView -> Pop x xsMaybe) xssMaybe) =
--     (x, xsMaybe, xssMaybe)

-- instance (Block xss, Block xs, Item xss ~ xs, Item xs ~ x) => Block (BlockBlock xss) where

--     length :: BlockBlock xss -> Positive
--     length = blockBlock >>> Block.length

--     concat :: NonEmpty (BlockBlock xss) -> BlockBlock xss
--     concat (x :| xs) = Foldable.foldl' (<>) x xs

--     leftView :: BlockBlock xss -> Pop (BlockBlock xss)
--     leftView (BlockBlock (leftView' -> (x, xsMaybe, xssMaybe))) =
--         Pop x $ BlockBlock <$> case xsMaybe of
--             Nothing -> xssMaybe
--             Just xs' -> Just $ Block.leftReview $ Pop xs' xssMaybe

--     divide :: forall a. (x -> Maybe a) -> BlockBlock xss -> Division a (BlockBlock xss)
--     divide f (BlockBlock xss) = fmap BlockBlock $ Division $ distribute _
--       where
--         distribute :: (Maybe xss, (Maybe xs, a, Maybe xs), Maybe xss) -> (Maybe xss, a, Maybe xss)
--         distribute (a, (a', x, b'), b) = (a <> a', x, b' <> b)

    -- span p = fmap coerce . span @(LL1 (Seq a)) p . coerce

    -- split n = fmap coerce . split @(LL1 (Seq a)) n . coerce

    -- take n = fmap coerce . take @(LL1 (Seq a)) n . coerce

    -- drop n = fmap coerce . drop @(LL1 (Seq a)) n . coerce

    -- while p = fmap coerce . while @(LL1 (Seq a)) p . coerce
