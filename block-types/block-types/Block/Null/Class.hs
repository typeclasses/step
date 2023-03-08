module Block.Null.Class
  (
    Null (..), Pivot (..), toNonEmpty, fromNonEmpty, notNullMaybe,
  )
  where

import Essentials

import Block.Class (End (..))
import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import Data.Char (Char)
import Data.Function (flip)
import Data.List.NonEmpty (NonEmpty)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Word (Word8)
import Integer (Natural, Positive)
import Prelude ((+), (-))

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.ByteString as ByteString
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as Monoid
import qualified Data.Semigroup as Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Integer.Natural as Natural
import qualified Integer.Positive as Positive

class Null x xs | xs -> x where
    toList :: End -> xs -> [x]
    fromList :: End -> [x] -> xs
    null :: xs -> Bool
    length :: xs -> Natural
    singleton :: x -> xs
    pop :: End -> xs -> Maybe (x, xs)
    push :: End -> x -> xs -> xs
    splitAt :: Natural -> xs -> (xs, xs)
    span :: Monad m => End -> (x -> m Bool) -> xs -> m (xs, xs)
    find :: Monad m => End -> (x -> m (Maybe found))
        -> xs -> m (Maybe (Pivot found xs))
    at :: End -> Positive -> xs -> Maybe x
    (++) :: xs -> xs -> xs
    concat :: End -> [xs] -> xs

data Pivot a xs = Pivot a (xs, xs) (xs, xs)

toNonEmpty :: Null x xs => End -> xs -> Maybe (NonEmpty x)
toNonEmpty end = toList end >>> NonEmpty.nonEmpty

fromNonEmpty :: Null x xs => End -> NonEmpty x -> xs
fromNonEmpty end = NonEmpty.toList >>> fromList end

notNullMaybe :: Null x xs => xs -> Maybe xs
notNullMaybe xs = if null xs then Nothing else Just xs

instance Null a (Seq a) where

    (++) :: Seq a -> Seq a -> Seq a
    (++) = (Semigroup.<>)

    concat :: End -> [Seq a] -> Seq a
    concat Front = Monoid.mconcat
    concat Back = Foldable.foldl' (flip (<>)) mempty

    at :: End -> Positive -> Seq a -> Maybe a
    at end n xs = (Seq.!?) xs i
      where
        n' = Positive.toInt n & Maybe.fromJust
        i = case end of Front -> n' - 1; Back -> Seq.length xs - n'

    toList :: End -> Seq a -> [a]
    toList Front = Foldable.toList
    toList Back = Foldable.foldl (flip (:)) []

    fromList :: End -> [a] -> Seq a
    fromList Front = Seq.fromList
    fromList Back = Seq.unfoldl (List.uncons >>> fmap \(xs, x) -> (x, xs))

    null :: Seq a -> Bool
    null = Seq.null

    length :: Seq a -> Natural
    length = Seq.length >>> Natural.fromInt >>> Maybe.fromJust

    singleton :: a -> Seq a
    singleton = Seq.singleton

    pop :: End -> Seq a -> Maybe (a, Seq a)
    pop _ Seq.Empty = Nothing
    pop Front (x Seq.:<| xs) = Just (x, xs)
    pop Back (xs Seq.:|> x) = Just (x, xs)

    push :: End -> a -> Seq a -> Seq a
    push Front x xs = x Seq.:<| xs
    push Back x xs = xs Seq.:|> x

    span :: Monad m => End -> (a -> m Bool) -> Seq a -> m (Seq a, Seq a)
    span Front f = go Seq.empty
      where
        go !seen xs = case Seq.viewl xs of
            Seq.EmptyL -> pure (seen, Seq.empty)
            (Seq.:<) y ys -> f y >>= \case
                False -> pure (seen, xs)
                True -> go ((Seq.:|>) seen y) ys
    span Back f = go Seq.empty
      where
        go !seen xs = case Seq.viewr xs of
            Seq.EmptyR -> pure (seen, Seq.empty)
            (Seq.:>) ys y -> f y >>= \case
                False -> pure (seen, xs)
                True -> go ((Seq.:<|) y seen) ys

    splitAt :: Natural -> Seq a -> (Seq a, Seq a)
    splitAt n = Seq.splitAt (Natural.toInt n & Maybe.fromJust)

    find :: Monad m => End -> (a -> m (Maybe found)) -> Seq a
        -> m (Maybe (Pivot found (Seq a)))
    find Front f = go Seq.empty
      where
        go !seen xs = case Seq.viewl xs of
          Seq.EmptyL -> pure Nothing
          (Seq.:<) y ys -> f y >>= \case
              Just found -> pure $ Just $ Pivot found ((Seq.:|>) seen y, ys) (seen, xs)
              Nothing -> go ((Seq.:|>) seen y) ys

    find Back f = go Seq.empty
      where
        go !seen xs = case Seq.viewr xs of
            Seq.EmptyR -> pure Nothing
            (Seq.:>) ys y -> f y >>= \case
                Just found -> pure $ Just $ Pivot found ((Seq.<|) y seen, ys) (seen, xs)
                Nothing -> go ((Seq.:<|) y seen) ys

instance Null Char Text where

    (++) :: Text -> Text -> Text
    (++) = (Semigroup.<>)

    concat :: End -> [Text] -> Text
    concat Front = Monoid.mconcat
    concat Back = List.reverse >>> Monoid.mconcat

    at :: End -> Positive -> Text -> Maybe Char
    at end n xs = if n' > len then Nothing else Just (Text.index xs i)
      where
        n' = Positive.toInt n & Maybe.fromJust
        len = Text.length xs
        i = case end of Front -> n' - 1; Back -> len - n'

    toList :: End -> Text -> [Char]
    toList Front = Text.unpack
    toList Back = go
      where
        go = maybe [] (\(xs, x) -> x : go xs) . Text.unsnoc

    fromList :: End -> [Char] -> Text
    fromList Front = Text.pack
    fromList Back = Text.pack >>> Text.reverse

    null :: Text -> Bool
    null = Text.null

    length :: Text -> Natural
    length = Text.length >>> Natural.fromInt >>> Maybe.fromJust

    singleton :: Char -> Text
    singleton = Text.singleton

    splitAt :: Natural -> Text -> (Text, Text)
    splitAt n = Text.splitAt (Natural.toInt n & Maybe.fromJust)

    pop :: End -> Text -> Maybe (Char, Text)
    pop Front = Text.uncons
    pop Back = Text.unsnoc >>> fmap \(a, b) -> (b, a)

    push :: End -> Char -> Text -> Text
    push Front = Text.cons
    push Back = flip Text.snoc

    span :: Monad m => End -> (Char -> m Bool) -> Text -> m (Text, Text)
    span Front = Text.spanM
    span Back = Text.spanEndM >>> (fmap . fmap) \(a, b) -> (b, a)

    find :: Monad m => End -> (Char -> m (Maybe found)) -> Text
        -> m (Maybe (Pivot found Text))
    find Front f xs =
        State.runStateT (Text.spanM s xs) Nothing
            <&> \((a, b), foundMaybe) ->
                (foundMaybe <&> \found -> Pivot found
                    (Text.snoc a (Text.head b), Text.tail b) (a, b))
      where
        s x = lift (f x) >>= \case
            Nothing -> pure True
            Just y -> State.put (Just y) $> False
    find Back f xs =
        State.runStateT (Text.spanEndM s xs) Nothing
            <&> \((a, b), foundMaybe) ->
                (foundMaybe <&> \found -> Pivot found
                    (Text.cons (Text.last a) b, Text.init a) (b, a))
      where
        s x = lift (f x) >>= \case
            Nothing -> pure True
            Just y -> State.put (Just y) $> False

instance Null Word8 ByteString where

    (++) :: ByteString -> ByteString -> ByteString
    (++) = (Semigroup.<>)

    concat :: End -> [ByteString] -> ByteString
    concat Front = Monoid.mconcat
    concat Back = List.reverse >>> Monoid.mconcat

    at :: End -> Positive -> ByteString -> Maybe Word8
    at end n xs = ByteString.indexMaybe xs i
      where
        n' = Positive.toInt n & Maybe.fromJust
        len = ByteString.length xs
        i = case end of Front -> n' - 1; Back -> len - n'

    toList :: End -> ByteString -> [Word8]
    toList Front = ByteString.unpack
    toList Back = go
      where
        go = maybe [] (\(xs, x) -> x : go xs) . ByteString.unsnoc

    fromList :: End -> [Word8] -> ByteString
    fromList Front = ByteString.pack
    fromList Back = ByteString.pack >>> ByteString.reverse

    null :: ByteString -> Bool
    null = ByteString.null

    length :: ByteString -> Natural
    length = ByteString.length >>> Natural.fromInt >>> Maybe.fromJust

    singleton :: Word8 -> ByteString
    singleton = ByteString.singleton

    splitAt :: Natural -> ByteString -> (ByteString, ByteString)
    splitAt n = ByteString.splitAt (Natural.toInt n & Maybe.fromJust)

    pop :: End -> ByteString -> Maybe (Word8, ByteString)
    pop Front = ByteString.uncons
    pop Back = ByteString.unsnoc >>> fmap \(a, b) -> (b, a)

    push :: End -> Word8 -> ByteString -> ByteString
    push Front = ByteString.cons
    push Back = flip ByteString.snoc

    span :: Monad m => End -> (Word8 -> m Bool) -> ByteString -> m (ByteString, ByteString)
    span end f xs =
        go 0 (toList end xs) <&> \i ->
        ByteString.splitAt (case end of Front -> i; Back -> ByteString.length xs - i) xs
        & case end of { Front -> id; Back -> \(a, b) -> (b, a) }
      where
        go !i ys = case ys of
            [] -> pure i
            z : zs -> f z >>= \case
                False -> pure i
                True -> go (i + 1) zs

    find :: Monad m => End -> (Word8 -> m (Maybe found)) -> ByteString
        -> m (Maybe (Pivot found ByteString))
    find end f xs =
        go 0 (toList end xs) <&> \(i, xm) ->
        xm <&> \x -> case end of
            Front -> Pivot x
                (ByteString.splitAt (i + 1) xs)
                (ByteString.splitAt (i    ) xs)
            Back -> Pivot x
                (swap $ ByteString.splitAt (ByteString.length xs - i - 1) xs)
                (swap $ ByteString.splitAt (ByteString.length xs - i    ) xs)
              where
                swap (a, b) = (b, a)
      where
        go !i ys = case ys of
            [] -> pure (i, Nothing)
            z : zs -> f z >>= \case
                Nothing -> go (i + 1) zs
                Just found -> pure (i, Just found)
