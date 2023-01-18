module Block.Text.Type
  (
    Text1 (..),
    assume,
  )
  where

import Essentials
import Block.Class

import Block.ListLike (NonEmptyListLike)
import Data.Char (Char)
import Data.Text (Text)
import Data.Coerce (coerce)

import qualified Block.ListLike.Unsafe as LL
import qualified Data.Foldable as Foldable
import qualified Data.Text as Text
import qualified Block.Class as Block

newtype Text1 = Text1 (NonEmptyListLike Text)
    deriving stock (Eq, Ord, Show)

instance Semigroup Text1 where
    Text1 a <> Text1 b = Text1 (a <> b)

instance Trivializable Text1 where
    refine = fmap Text1 . refine
    generalize (Text1 x) = generalize x

type instance Block.Item Text1 = Char

type instance Nullable Text1 = Text

instance Block Text1 where

    -- Takes advantage of Text's faster concat function
    concat = Text1 . LL.assume . Text.concat . fmap generalize . Foldable.toList

    -- The rest are just coercions of NonEmptyListLike methods
    leftView = (\(Pop i r) -> Pop i (coerce r)) . leftView @(NonEmptyListLike Text) . coerce
    span p = fmap coerce . span @(NonEmptyListLike Text) p . coerce
    split n = fmap coerce . split @(NonEmptyListLike Text) n . coerce
    take n = fmap coerce . take @(NonEmptyListLike Text) n . coerce
    drop n = fmap coerce . drop @(NonEmptyListLike Text) n . coerce
    while p = fmap coerce . while @(NonEmptyListLike Text) p . coerce
    length = length @(NonEmptyListLike Text) . coerce

assume :: Text -> Text1
assume = Text1 . LL.assume
