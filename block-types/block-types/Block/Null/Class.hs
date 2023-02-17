module Block.Null.Class
  (
    Null (..), toNonEmpty, fromNonEmpty, notNullMaybe,
  )
  where

import Essentials

import Data.List.NonEmpty (NonEmpty)
import Integer (Natural)
import Block.Class (End (..))
import Data.Function (flip)
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Char (Char)
import Data.Text (Text)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Integer.Natural as Natural
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text

class (Monoid xs) => Null x xs | xs -> x where
    toList :: xs -> [x]
    fromList :: [x] -> xs
    null :: xs -> Bool
    length :: xs -> Natural
    singleton :: x -> xs
    splitAt :: Natural -> xs -> (xs, xs)
    pop :: End -> xs -> Maybe (x, xs)
    push :: End -> x -> xs -> xs
    span :: End -> (x -> Bool) -> xs -> (xs, xs)
    find :: End -> (x -> Maybe found) -> xs -> Maybe (xs, found, xs)

toNonEmpty :: Null x xs => xs -> Maybe (NonEmpty x)
toNonEmpty = toList >>> NonEmpty.nonEmpty

fromNonEmpty :: Null x xs => NonEmpty x -> xs
fromNonEmpty = NonEmpty.toList >>> fromList

notNullMaybe :: Null x xs => xs -> Maybe xs
notNullMaybe xs = if null xs then Nothing else Just xs

instance Null Char Text where
    toList = Text.unpack
    fromList = Text.pack
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

instance Null Word8 ByteString where
    toList = ByteString.unpack
    fromList = ByteString.pack
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

swap :: (b, a) -> (a, b)
swap (a, b) = (b, a)
