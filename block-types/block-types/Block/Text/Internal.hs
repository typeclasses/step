module Block.Text.Internal
  (
    {- * Type -} Text1 (..),
  )
  where

import Essentials
import Block.Class

import Block.ListLike (LL1)
import Data.Char (Char)
import Data.Text (Text)
import Data.Coerce (coerce)

import qualified Data.Foldable as Foldable
import qualified Data.Text as Text
import qualified Block.Class as Block

newtype Text1 = Text1 (LL1 Text)
    deriving stock (Eq, Ord, Show)

instance Semigroup Text1 where
    Text1 a <> Text1 b = Text1 (a <> b)

instance Trivializable Text1 where
    refine = fmap Text1 . refine
    generalize (Text1 x) = generalize x
    assume = Text1 . assume

type instance Block.Item Text1 = Char

type instance Nullable Text1 = Text

instance Block Text1 where

    -- Takes advantage of Text's faster concat function
    concat = Text1 . assume . Text.concat . fmap generalize . Foldable.toList

    -- The rest are just coercions of LL1 methods
    singleton = coerce . singleton @(LL1 Text)
    leftView = (\(Pop i r) -> Pop i (coerce r)) . leftView @(LL1 Text) . coerce
    leftReview = coerce . leftReview @(LL1 Text) . (\(Pop i r) -> Pop i (coerce r))
    span p = fmap coerce . span @(LL1 Text) p . coerce
    divide f = fmap coerce . divide @(LL1 Text) f . coerce
    split n = fmap coerce . split @(LL1 Text) n . coerce
    take n = fmap coerce . take @(LL1 Text) n . coerce
    drop n = fmap coerce . drop @(LL1 Text) n . coerce
    while p = fmap coerce . while @(LL1 Text) p . coerce
    length = length @(LL1 Text) . coerce
