module Block.Text.Type
  (
    {- * Type -} Text1,
  )
  where

import Essentials
import Block.Class.ClassNames

import Data.Char (Char)
import Data.Text (Text)
import Block.Null.Type (NotNull)

newtype Text1 = Text1 (NotNull Char Text)
  deriving newtype
    (
      Eq, Ord, Show, Semigroup, Singleton Char, Positional,
      Search Char, NonEmptyIso Char, Refined Text, Index Char, Concat
    )
