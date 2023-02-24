module Block.ByteString.Type
  (
    {- * Type -} ByteString1,
  )
  where

import Essentials
import Block.Class.ClassNames

import Block.Null.Type (NotNull)
import Data.ByteString (ByteString)
import Data.String (IsString)
import Data.Word (Word8)

newtype ByteString1 = ByteString1 (NotNull Word8 ByteString)
  deriving newtype
    (
      Eq, Ord, Show, Semigroup, Singleton Word8, Positional,
      Search Word8, NonEmptyIso Word8, Refined ByteString,
      Index Word8, Concat, ItemEquality, IsString
    )
