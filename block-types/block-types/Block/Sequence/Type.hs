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

newtype Seq1 a = Seq1 (NotNull a (Seq a))
  deriving newtype
    (
      Eq, Ord, Show, Semigroup, Singleton a, Positional,
      Search a, NonEmptyIso a, Refined (Seq a), Index a,
      Concat, ItemEquality
    )

instance IsString (Seq1 Char) where

    fromString :: String -> Seq1 Char
    fromString = Seq1 . fromString
