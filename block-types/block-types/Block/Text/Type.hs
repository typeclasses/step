module Block.Text.Type
  (
    {- * Type -} Text1,
  )
  where

import Essentials
import Block.Class

import Block.Null.Type (NotNull)
import Data.Char (Char)
import Data.String (IsString)
import Data.Text (Text)
import GHC.Exts (IsList (..), Item)
import Prelude (error)
import Data.List.NonEmpty (nonEmpty)

import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe

newtype Text1 = Text1 (NotNull Char Text)
  deriving newtype
    (
      Eq, Ord, Show, Semigroup, Block Char, Singleton Char, Positional,
      Search Char, Enumerate Char, Construct Char, Refined Text, Index Char,
      Concat, ItemEquality, IsString
    )

instance IsList Text1 where

    type Item Text1 = Char

    toList =
        toNonEmpty Front >>> Foldable.toList

    fromList =
        nonEmpty
        >>> Maybe.fromMaybe (error "fromList Text1: empty")
        >>> fromNonEmpty Front
