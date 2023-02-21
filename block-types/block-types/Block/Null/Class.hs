module Block.Null.Class
  (
    Null (..), toNonEmpty, fromNonEmpty, notNullMaybe,
  )
  where

import Essentials

import Data.List.NonEmpty (NonEmpty)
import Integer (Natural)
import Block.Class (End (..))
import Data.Function (flip, fix)
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Char (Char)
import Data.Text (Text)
import Prelude ((+))
import Data.Sequence (Seq)

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Integer.Natural as Natural
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Sequence as Seq

class (Monoid xs) => Null x xs | xs -> x where
    toList :: End -> xs -> [x]
    fromList :: End -> [x] -> xs
    null :: xs -> Bool
    length :: xs -> Natural
    singleton :: x -> xs
    pop :: End -> xs -> Maybe (x, xs)
    push :: End -> x -> xs -> xs
    span :: End -> (x -> Bool) -> xs -> (xs, xs)
    splitAt :: Natural -> xs -> (xs, xs)
    find :: End -> (x -> Maybe found) -> xs -> Maybe (xs, found, xs)

toNonEmpty :: Null x xs => End -> xs -> Maybe (NonEmpty x)
toNonEmpty end = toList end >>> NonEmpty.nonEmpty

fromNonEmpty :: Null x xs => End -> NonEmpty x -> xs
fromNonEmpty end = NonEmpty.toList >>> fromList end

notNullMaybe :: Null x xs => xs -> Maybe xs
notNullMaybe xs = if null xs then Nothing else Just xs

instance Null a (Seq a) where
    toList end = _

instance Null Char Text where
    toList Front = Text.unpack
    toList Back = Text.foldr (:) []
    fromList Front = Text.pack
    fromList Back = Text.pack >>> Text.reverse
    null = Text.null
    length = Text.length >>> Natural.fromInt >>> Maybe.fromJust
    singleton = Text.singleton
    splitAt n = Text.splitAt (Natural.toInt n & Maybe.fromJust)
    pop Front = Text.uncons
    pop Back = Text.unsnoc >>> fmap swap
    push Front = Text.cons
    push Back = flip Text.snoc
    span Front p = Text.span p
    span Back p = Text.spanEndM (Identity . p) >>> runIdentity
    find Front f =
        (Text.spanM (\x -> case f x of
            Nothing -> pure True
            Just y -> State.put (Just y) $> False))
        >>> flip State.runState Nothing
        >>> (\((a, b), foundMaybe) -> foundMaybe <&> \found -> (a, found, Text.tail b))
    find Back f =
        (Text.spanEndM \x -> case f x of
            Nothing -> pure True
            Just y -> State.put (Just y) $> False)
        >>> flip State.runState Nothing
        >>> \((a, b), foundMaybe) -> foundMaybe <&> \found -> (b, found, Text.init a)

instance Null Word8 ByteString where
    toList Front = ByteString.unpack
    toList Back = ByteString.foldr (:) []
    fromList Front = ByteString.pack
    fromList Back = ByteString.pack >>> ByteString.reverse
    null = ByteString.null
    length = ByteString.length >>> Natural.fromInt >>> Maybe.fromJust
    singleton = ByteString.singleton
    splitAt n = ByteString.splitAt (Natural.toInt n & Maybe.fromJust)
    pop Front = ByteString.uncons
    pop Back = ByteString.unsnoc >>> fmap swap
    push Front = ByteString.cons
    push Back = flip ByteString.snoc
    span Front p = ByteString.span p
    span Back p = ByteString.spanEnd p >>> swap
    find Front f xs = ByteString.findIndex (f >>> Maybe.isJust) xs <&> \i ->
        ( ByteString.take i xs
        , Maybe.fromJust (f (ByteString.index xs i))
        , ByteString.drop (i + 1) xs )
    find Back f xs = ByteString.findIndexEnd (f >>> Maybe.isJust) xs <&> \i ->
        ( ByteString.drop (i + 1) xs
        , Maybe.fromJust (f (ByteString.index xs i))
        , ByteString.take i xs )

swap :: (b, a) -> (a, b)
swap (a, b) = (b, a)
