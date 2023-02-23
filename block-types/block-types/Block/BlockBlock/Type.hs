module Block.BlockBlock.Type
  (
    BlockBlock (BlockBlock),
  )
  where

import Essentials
import Block.Class

import Integer (Positive)
import Prelude ((+))
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup (sconcat)
import Integer.Signed (Signed (..))

import qualified Data.Maybe as Maybe
import qualified Fold.Nonempty as Fold
import qualified Block.Class.End as End
import qualified Integer.Positive as Positive

data BlockBlock x xs xss = BlockBlockUnsafe{ bbXss :: !xss, bbLength :: !Positive }
    deriving stock (Eq, Ord, Show)

pattern BlockBlock :: forall x xs xss. (NonEmptyIso xs xss, Positional xs) =>
    xss -> BlockBlock x xs xss
pattern BlockBlock xss <- BlockBlockUnsafe xss _
  where
    BlockBlock xss = BlockBlockUnsafe xss
      (Fold.run Fold.sum (length <$> (toNonEmpty Front xss :: NonEmpty xs)))

{-# complete BlockBlock #-}

instance (Concat xss) => Concat (BlockBlock x xs xss) where

    (++) :: BlockBlock x xs xss -> BlockBlock x xs xss -> BlockBlock x xs xss
    BlockBlockUnsafe xss1 len1 ++ BlockBlockUnsafe xss2 len2 =
        BlockBlockUnsafe (xss1 ++ xss2) (len1 + len2)

    concat :: End -> NonEmpty (BlockBlock x xs xss) -> BlockBlock x xs xss
    concat end bbs = BlockBlockUnsafe (concat end (bbs <&> bbXss)) (Fold.run Fold.sum (bbs <&> bbLength))

instance (NonEmptyIso x xs, NonEmptyIso xs xss, Singleton xs xss, Positional xs) =>
        NonEmptyIso x (BlockBlock x xs xss) where

    toNonEmpty :: End -> BlockBlock x xs xss -> NonEmpty x
    toNonEmpty end = bbXss >>> toNonEmpty end >>> fmap (toNonEmpty end) >>> sconcat

    fromNonEmpty :: End -> NonEmpty x -> BlockBlock x xs xss
    fromNonEmpty end = fromNonEmpty end >>> singleton >>> BlockBlock

instance (Singleton x xs, Singleton xs xss) => Singleton x (BlockBlock x xs xss) where

    singleton :: x -> BlockBlock x xs xss
    singleton x = BlockBlockUnsafe (singleton (singleton x)) 1

    pop :: End -> BlockBlock x xs xss -> Pop x (BlockBlock x xs xss)
    pop end (BlockBlockUnsafe xss n) = Pop x remainder
      where
        Pop xs xssMaybe = pop end xss
        Pop x  xsMaybe  = pop end xs
        remainder = pushMaybe end xsMaybe xssMaybe
            <&> \xss' -> BlockBlockUnsafe xss'
              (n & Positive.subtractOne & Positive.fromNatural & Maybe.fromJust)

    push :: End -> x -> BlockBlock x xs xss -> BlockBlock x xs xss
    push end x (BlockBlockUnsafe xss n) =
        BlockBlockUnsafe (push end (singleton x) xss) (n + 1)

instance (Search xs xss, Singleton xs xss, Positional xs, NonEmptyIso xs xss) =>
        Positional (BlockBlock x xs xss) where

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
                    (pushMaybe (End.opposite end) x1 xs1, pushMaybe end x2 xs2)
              where
                f xs = do
                    rem <- get
                    case take end rem xs of
                        TakeInsufficient (Shortfall s)  ->  put s $> Nothing
                        TakeAll                         ->  pure $ Just (Just xs, Nothing)
                        TakePart a b                    ->  pure $ Just (Just a, Just b)

instance (Search xs xss, Singleton xs xss, NonEmptyIso xs xss, Index x xs) =>
        Index x (BlockBlock x xs xss) where

    at :: End -> Positive -> BlockBlock x xs xss -> Maybe x
    at end = \n (BlockBlock xss) -> go n xss
      where
        go :: Positive -> xss -> Maybe x
        go n xss = case Positive.subtract (length xs) n of
            Minus n' -> xss' & maybe Nothing (go n')
            NotMinus _ -> at end n xs
          where
            Pop xs xss' = pop end xss

instance (Search xs xss, Search x xs, NonEmptyIso xs xss, Positional xs, Singleton xs xss,
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
                case pushMaybe Front a' a of
                    Nothing -> SpanNone
                    Just a'' -> SpanPart
                        (BlockBlock a'')
                        (BlockBlock (maybe (singleton b') (push end b') b))

    find :: End -> (x -> State s (Maybe found)) -> BlockBlock x xs xss
        -> State s (Maybe (Pivot found (BlockBlock x xs xss)))
    find end f bb =
        bb & bbXss
        & find end (find end f)
        <&> fmap \(Pivot a (Pivot a' x b') b) ->
                Pivot
                    (BlockBlock <$> pushMaybe (End.opposite end) a' a)
                    x
                    (BlockBlock <$> pushMaybe end b' b)
