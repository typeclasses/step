module Block.Text.Type
  (
    {- * Type -} Text1,
  )
  where

import Essentials
import Block.Class.ClassNames

import Block.Null.Type (NotNull)
import Data.Char (Char)
import Data.String (IsString)
import Data.Text (Text)

newtype Text1 = Text1 (NotNull Char Text)
  deriving newtype
    (
      Eq, Ord, Show, Semigroup, Singleton Char, Positional,
      Search Char, Enumerate Char, NonEmptyIso Char, Refined Text, Index Char,
      Concat, ItemEquality, IsString
    )
