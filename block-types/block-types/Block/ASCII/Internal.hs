module Block.ASCII.Internal
  (
    {- * Type -} ASCII1 (..), ASCII (..),
    {- * Utilities -} generalizeAscii, refineAscii, assumeAscii,
            ascii1Lower, ascii1Upper, asciiLower, asciiUpper,
  )
  where

import Essentials
import Block.Class

import Block.ByteString.Type (ByteString1)
import Data.ByteString (ByteString)
import Data.String (IsString)
import ASCII.Char (Char)
import Integer (Positive)
import Data.List.NonEmpty (NonEmpty)
import Data.Word (Word8)
import Prelude ((+), (-))
import Data.Bool ((&&))

import qualified Data.Maybe as Maybe
import qualified Data.ByteString as BS
import qualified ASCII.Char as A

newtype ASCII1 = ASCII1 ByteString1
  deriving newtype (Eq, Ord, Show, Semigroup, IsString, Concat, ItemEquality)

instance Block Char ASCII1

instance Singleton Char ASCII1 where

    singleton :: Char -> ASCII1
    singleton = ASCII1 . singleton . A.toWord8

    pop :: End -> ASCII1 -> Pop Char ASCII1
    pop end (ASCII1 bs) =
        let Pop x xsm = pop end bs in
        Pop (A.fromWord8Unsafe x) (ASCII1 <$> xsm)

    push :: End -> Char -> ASCII1 -> ASCII1
    push end x (ASCII1 bs) = ASCII1 (push end (A.toWord8 x) bs)

instance Positional ASCII1 where

    length :: ASCII1 -> Positive
    length (ASCII1 bs) = length bs

    take :: End -> Positive -> ASCII1 -> Take ASCII1
    take end n (ASCII1 bs) = take end n bs <&> ASCII1

instance Search Char ASCII1 where

    span :: End -> (Char -> State s Bool) -> ASCII1 -> State s (Span ASCII1)
    span end f (ASCII1 bs) = span end (f . A.fromWord8Unsafe) bs <&> fmap ASCII1

    find :: End -> (Char -> State s (Maybe found)) -> ASCII1
         -> State s (Maybe (Pivot found ASCII1))
    find end f (ASCII1 bs) =
        find end (f . A.fromWord8Unsafe) bs <&> (fmap . fmap) ASCII1

instance Enumerate Char ASCII1 where

    toNonEmpty :: End -> ASCII1 -> NonEmpty Char
    toNonEmpty end (ASCII1 bs) = toNonEmpty end bs <&> A.fromWord8Unsafe

    foldItems :: End -> (Char -> a) -> (Char -> State a ()) -> ASCII1 -> a
    foldItems end initial step (ASCII1 bs) =
        foldItems end (initial . A.fromWord8Unsafe) (step . A.fromWord8Unsafe) bs

instance NonEmptyIso Char ASCII1 where

    fromNonEmpty :: End -> NonEmpty Char -> ASCII1
    fromNonEmpty end = fmap A.toWord8 >>> fromNonEmpty end >>> ASCII1

instance Index Char ASCII1 where

    at :: End -> Positive -> ASCII1 -> Maybe Char
    at end n (ASCII1 bs) = at end n bs <&> A.fromWord8Unsafe

---

newtype ASCII = ASCII ByteString
  deriving newtype (Eq, Ord, Show, Semigroup, IsString)

generalizeAscii :: ASCII -> ByteString
generalizeAscii (ASCII x) = x

refineAscii :: ByteString -> Maybe ASCII
refineAscii x = if BS.all (< 128) x then Just (ASCII x) else Nothing

assumeAscii :: ByteString -> ASCII
assumeAscii = refineAscii >>> Maybe.fromJust

instance Refined ASCII ASCII1 where

    generalize :: ASCII1 -> ASCII
    generalize (ASCII1 x) = ASCII (generalize x)

    refine :: ASCII -> Maybe ASCII1
    refine (ASCII x) = ASCII1 <$> refine x

ascii1Lower :: ASCII1 -> ASCII1
ascii1Lower (ASCII1 bs) = ASCII1 (bs & (generalize >>> BS.map word8Lower >>> assume ))

ascii1Upper :: ASCII1 -> ASCII1
ascii1Upper (ASCII1 bs) = ASCII1 (bs & (generalize >>> BS.map word8Upper >>> assume ))

asciiLower :: ASCII -> ASCII
asciiLower (ASCII bs) = ASCII (BS.map word8Lower bs)

asciiUpper :: ASCII -> ASCII
asciiUpper (ASCII bs) = ASCII (BS.map word8Upper bs)

word8Lower :: Word8 -> Word8
word8Lower x = if x >= 65 && x <= 90 then x + 32 else x

word8Upper :: Word8 -> Word8
word8Upper x = if x >= 97 && x <= 122 then x - 32 else x
