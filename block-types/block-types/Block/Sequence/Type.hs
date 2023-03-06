module Block.Sequence.Type
  (
    {- * Type -} Seq1,
  )
  where

import Essentials
import Block.Class

import Block.Null.Type (NotNull)
import Data.Char (Char)
import Data.Sequence (Seq)
import Data.String (IsString (..), String)
import GHC.Exts (IsList (..), Item)
import Prelude (error)
import Data.List.NonEmpty (nonEmpty)

import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe

newtype Seq1 a = Seq1 (NotNull a (Seq a))
  deriving newtype
    (
      Eq, Ord, Show, Semigroup, Block a, Singleton a, Positional,
      Search a, Enumerate a, Construct a, Refined (Seq a), Index a,
      Concat, ItemEquality
    )

instance IsList (Seq1 a) where

    type Item (Seq1 a) = a

    toList =
        toNonEmpty Front >>> Foldable.toList

    fromList =
        nonEmpty
        >>> Maybe.fromMaybe (error "fromList Seq1: empty")
        >>> fromNonEmpty Front

instance IsString (Seq1 Char) where

    fromString :: String -> Seq1 Char
    fromString = Seq1 . fromString
