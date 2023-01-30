module Block.ByteString.Type
  (
    {- * Type -} ByteString1 (..),
  )
  where

import Essentials
import Block.Class

import Block.ListLike (LL1)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Word (Word8)

import qualified Data.Foldable as Foldable
import qualified Data.ByteString as ByteString
import qualified Block.Class as Block

newtype ByteString1 = ByteString1 (LL1 ByteString)
    deriving stock (Eq, Ord, Show)

instance Semigroup ByteString1 where
    ByteString1 a <> ByteString1 b = ByteString1 (a <> b)

instance Trivializable ByteString1 where
    refine = fmap ByteString1 . refine
    generalize (ByteString1 x) = generalize x
    assume = ByteString1 . assume

type instance Block.Item ByteString1 = Word8

type instance Nullable ByteString1 = ByteString

instance Block ByteString1 where

    -- Takes advantage of ByteString's faster concat function
    concat = ByteString1 . assume . ByteString.concat . fmap generalize . Foldable.toList

    -- The rest are just coercions of LL1 methods
    leftView = (\(Pop i r) -> Pop i (coerce r)) . leftView @(LL1 ByteString) . coerce
    span p = fmap coerce . span @(LL1 ByteString) p . coerce
    split n = fmap coerce . split @(LL1 ByteString) n . coerce
    take n = fmap coerce . take @(LL1 ByteString) n . coerce
    drop n = fmap coerce . drop @(LL1 ByteString) n . coerce
    while p = fmap coerce . while @(LL1 ByteString) p . coerce
    length = length @(LL1 ByteString) . coerce
