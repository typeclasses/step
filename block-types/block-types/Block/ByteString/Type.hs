module Block.ByteString.Type
  (
    {- * Type -} ByteString1,
  )
  where

import Essentials
import Block.Class.ClassNames

import Data.ByteString (ByteString)
import Data.Word (Word8)
import Block.Null.Type (NotNull)

newtype ByteString1 = ByteString1 (NotNull Word8 ByteString)
  deriving newtype
    (
      Eq, Ord, Show, Semigroup, Singleton Word8, Positional Word8,
      Search Word8, NonEmptyIso Word8, Refined Word8 ByteString
    )
