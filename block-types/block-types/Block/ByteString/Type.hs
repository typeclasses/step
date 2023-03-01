module Block.ByteString.Type
  (
    {- * Type -} ByteString1,
  )
  where

import Essentials
import Block.Class.ClassNames

import Block.Class (toNonEmpty, End (..), fromNonEmpty)
import Block.Null.Type (NotNull)
import Data.ByteString (ByteString)
import Data.String (IsString)
import Data.Word (Word8)
import GHC.Exts (IsList (..), Item)
import Prelude (error)
import Data.List.NonEmpty (nonEmpty)

import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe

newtype ByteString1 = ByteString1 (NotNull Word8 ByteString)
  deriving newtype
    (
      Eq, Ord, Show, Semigroup, Block Word8, Singleton Word8, Positional,
      Search Word8, Enumerate Word8, NonEmptyIso Word8, Refined ByteString,
      Index Word8, Concat, ItemEquality, IsString
    )

instance IsList ByteString1 where

    type Item ByteString1 = Word8

    toList =
        toNonEmpty Front >>> Foldable.toList

    fromList =
        nonEmpty
        >>> Maybe.fromMaybe (error "fromList ByteString1: empty")
        >>> fromNonEmpty Front
