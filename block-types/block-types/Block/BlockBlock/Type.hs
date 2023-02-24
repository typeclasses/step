module Block.BlockBlock.Type
  (
    BlockBlock (BlockBlock),
  )
  where

import Essentials
import Block.Class

import Data.List.NonEmpty (NonEmpty (..))
import Data.String (IsString (..), String)
import Integer (Positive)
import Integer.Signed (Signed (..))
import Prelude ((+))
import Data.Bool ((&&))

import qualified Block.Class.End as End
import qualified Data.Maybe as Maybe
import qualified Fold.Nonempty as Fold
import qualified Integer.Positive as Positive

data BlockBlock x xs xss = BlockBlockUnsafe{ bbXss :: !xss, bbLength :: !Positive }
    deriving stock (Eq, Ord, Show)


---  Pattern  ---

pattern BlockBlock :: forall x xs xss. (NonEmptyIso xs xss, Positional xs) =>
    xss -> BlockBlock x xs xss
pattern BlockBlock xss <- BlockBlockUnsafe xss _
  where
    BlockBlock xss = BlockBlockUnsafe xss
      (Fold.run Fold.sum (length <$> (toNonEmpty Front xss :: NonEmpty xs)))

{-# complete BlockBlock #-}


---  IsString  ---

instance (IsString xs, Singleton xs xss, NonEmptyIso xs xss, Positional xs) =>
        IsString (BlockBlock x xs xss) where

    fromString :: String -> BlockBlock x xs xss
    fromString = fromString >>> singleton >>> BlockBlock


---  ItemEquality  ---

instance (Positional xs, ItemEquality xs, Singleton xs xss) =>
        ItemEquality (BlockBlock x xs xss) where

    sameItems :: BlockBlock x xs xss -> BlockBlock x xs xss -> Bool
    sameItems = \(BlockBlockUnsafe xss1 len1) (BlockBlockUnsafe xss2 len2) ->
                    len1 == len2 && go xss1 xss2
      where
        end = Front
        go :: xss -> xss -> Bool
        go (pop end -> Pop xs1 mXss1) (pop end -> Pop xs2 mXss2) =
            case biPrefix itemEquality end (xs1, xs2) of
                NoPrefixRelation -> False
                BothPrefix -> case (mXss1, mXss2) of
                    (Nothing, Nothing) -> True
                    (Just xss1, Just xss2) -> go xss1 xss2
                    _ -> False
                IsPrefix First _ remainder -> case mXss1 of
                    Nothing -> False
                    Just xss1 -> go xss1 (unpop end (Pop remainder mXss2))
                IsPrefix Second _ remainder -> case mXss2 of
                    Nothing -> False
                    Just xss2 -> go (unpop end (Pop remainder mXss1)) xss2


---  Concat  ---

instance (Positional xs, ItemEquality xs, Singleton xs xss, Concat xss) => Concat (BlockBlock x xs xss) where

    (++) :: BlockBlock x xs xss -> BlockBlock x xs xss -> BlockBlock x xs xss
    BlockBlockUnsafe xss1 len1 ++ BlockBlockUnsafe xss2 len2 =
        BlockBlockUnsafe (xss1 ++ xss2) (len1 + len2)

    concat :: End -> NonEmpty (BlockBlock x xs xss) -> BlockBlock x xs xss
    concat end bbs = BlockBlockUnsafe (concat end (bbs <&> bbXss)) (Fold.run Fold.sum (bbs <&> bbLength))


---  Enumerate  ---

instance (Positional xs, ItemEquality xs, Singleton xs xss, Singleton x xs, Enumerate xs xss, Enumerate x xs) =>
        Enumerate x (BlockBlock x xs xss) where

    foldItems :: End -> (x -> a) -> (a -> x -> a) -> BlockBlock x xs xss -> a
    foldItems end initialX stepX = bbXss >>>
        foldItems end
            (foldItems end initialX stepX)
            (\a -> foldItems end (stepX a) stepX)

    toNonEmpty :: End -> BlockBlock x xs xss -> NonEmpty x
    toNonEmpty end = bbXss >>>
        \(pop end -> Pop (pop end -> Pop x xs) xss) -> x :| f xs xss
      where

        f :: Maybe xs -> Maybe xss -> [x]
        f Nothing xss = g xss
        f (Just (pop end -> Pop x xs)) xss = x : f xs xss

        g :: Maybe xss -> [x]
        g Nothing = []
        g (Just (pop end -> Pop (pop end -> Pop x xs) xss)) =
            x : f xs xss


---  NonEmptyIso  ---

instance (Eq x, NonEmptyIso x xs, NonEmptyIso xs xss, Singleton xs xss, Positional xs, Singleton x xs) =>
        NonEmptyIso x (BlockBlock x xs xss) where

    fromNonEmpty :: End -> NonEmpty x -> BlockBlock x xs xss
    fromNonEmpty end = fromNonEmpty end >>> singleton >>> BlockBlock


---  Singleton  ---

instance (Singleton x xs, Singleton xs xss, Positional xs, ItemEquality xs) => Singleton x (BlockBlock x xs xss) where

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


---  Positional  ---

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


---  Index  ---

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


---  Search  ---

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
                case pushMaybe (End.opposite end) a' a of
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
