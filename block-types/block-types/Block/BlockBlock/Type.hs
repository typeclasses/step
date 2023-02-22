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
import Control.Applicative (liftA2)

import qualified Data.Foldable as Foldable
import qualified Block.Class as Block
import qualified Data.Maybe as Maybe
import qualified Fold.Nonempty as Fold
import qualified Integer.Positive as Positive

data BlockBlock x xs xss = BlockBlockUnsafe{ bbXss :: !xss, bbLength :: !Positive }
    deriving stock (Eq, Ord, Show)

pattern BlockBlock :: forall x xs xss. (NonEmptyIso xs xss, Positional x xs) =>
    xss -> BlockBlock x xs xss
pattern BlockBlock xss <- BlockBlockUnsafe xss _
  where
    BlockBlock xss = BlockBlockUnsafe xss
      (Fold.run Fold.sum (length <$> (toNonEmpty Front xss :: NonEmpty xs)))

instance (Semigroup xss) => Semigroup (BlockBlock x xs xss) where
    BlockBlockUnsafe xss1 len1 <> BlockBlockUnsafe xss2 len2 =
        BlockBlockUnsafe (xss1 <> xss2) (len1 + len2)

instance (NonEmptyIso x xs, NonEmptyIso xs xss, Singleton xs xss, Positional x xs) =>
        NonEmptyIso x (BlockBlock x xs xss) where

    toNonEmpty :: End -> BlockBlock x xs xss -> NonEmpty x
    toNonEmpty end = bbXss >>> toNonEmpty end >>> fmap (toNonEmpty end) >>> sconcat

    fromNonEmpty :: End -> NonEmpty x -> BlockBlock x xs xss
    fromNonEmpty end = fromNonEmpty end >>> singleton >>> BlockBlock

instance (Search xs xss, Singleton xs xss, Positional x xs) => Positional x (BlockBlock x xs xss) where

    length :: BlockBlock x xs xss -> Positive
    length = bbLength

    take :: End -> Positive -> BlockBlock x xs xss -> Take (BlockBlock x xs xss)
    take end n (BlockBlockUnsafe xss len) = case Positive.subtract len n of
        Minus s -> TakeInsufficient (Shortfall s)
        Zero -> TakeAll
        Plus remainderLength ->
            TakePart
                (BlockBlockUnsafe (Maybe.fromJust taken) n)
                (BlockBlockUnsafe (Maybe.fromJust remainder) remainderLength)
          where
            (taken, remainder) = evalState n (find end f xss) & Maybe.fromJust
                & \(Pivot xs1 (x1, x2) xs2) ->
                    (liftA2 (push Back) x1 xs1, liftA2 (push Front) x2 xs2)
              where
                f xs = do
                    rem <- get
                    case take Front rem xs of
                        TakeInsufficient (Shortfall s)  ->  put s $> Nothing
                        TakeAll                         ->  pure $ Just (Nothing, Nothing)
                        TakePart a b                    ->  pure $ Just (Just a, Just b)

instance (Search xs xss, Search x xs, NonEmptyIso xs xss, Positional x xs, Singleton xs xss,
        Semigroup xss) => Search x (BlockBlock x xs xss) where

    span :: End -> (x -> State s Bool) -> BlockBlock x xs xss
        -> State s (Span (BlockBlock x xs xss))
    span end f bb =
        bb & bbXss
        & find end
            (\xs -> span end f xs <&> \case
                SpanAll       ->  Nothing
                SpanNone      ->  Just (Nothing, xs)
                SpanPart a b  ->  Just (Just a, b)
            )
        <&> \case
            Nothing -> SpanAll
            Just (Pivot a (a', b') b) ->
                case pushMaybe Back a' a of
                    Nothing -> SpanNone
                    Just a'' -> SpanPart
                        (BlockBlock a'')
                        (BlockBlock (maybe (singleton b') (push Front b') b))

    find :: End -> (x -> State s (Maybe found)) -> BlockBlock x xs xss
        -> State s (Maybe (Pivot found (BlockBlock x xs xss)))
    find end f bb =
        bb & bbXss
        & find end (find end f)
        <&> fmap \(Pivot a (Pivot a' x b') b) ->
                Pivot
                    (BlockBlock <$> pushMaybe Back a' a)
                    x
                    (BlockBlock <$> pushMaybe Front b' b)

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
