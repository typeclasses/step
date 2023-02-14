module Block.Class.Text.Internal
  (
    {- * Type -} Text1,
  )
  where

import Essentials
import Block.Class

import Data.Char (Char)
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty ((:|)))

import qualified Data.Foldable as Foldable
import qualified Data.Text as Text
import qualified Block.Class as Block

newtype Text1 = Text1 Text
    deriving stock (Eq, Ord, Show)

instance Semigroup Text1 where
    Text1 a <> Text1 b = Text1 (a <> b)

instance Refined Text1 where
    refine x = if Text.null x then Nothing else Just (Text1 x)
    generalize (Text1 x) = x
    assume = Text1

type instance Item Text1 = Char

type instance Nullable Text1 = Text

instance Block Text1

instance Singleton Text1 where

    singleton = Text.singleton >>> assume

    push Front x (Text1 xs) = assume (Text.cons x xs)
    push Back x (Text1 xs) = assume (Text.snoc xs x)

instance Positional Text1 where

instance Search Text1 where

instance NonEmptyIso Text1 where
    toNonEmpty = generalize >>> Text.unpack >>> assume
    fromNonEmpty = Foldable.toList >>> Text.pack >>> assume

    -- leftView = (\(Pop i r) -> Pop i (coerce r)) . leftView @(LL1 Text) . coerce
    -- leftReview = coerce . leftReview @(LL1 Text) . (\(Pop i r) -> Pop i (coerce r))
    -- span p = fmap coerce . span @(LL1 Text) p . coerce
    -- divide f = fmap coerce . divide @(LL1 Text) f . coerce
    -- split n = fmap coerce . split @(LL1 Text) n . coerce
    -- take n = fmap coerce . take @(LL1 Text) n . coerce
    -- drop n = fmap coerce . drop @(LL1 Text) n . coerce
    -- while p = fmap coerce . while @(LL1 Text) p . coerce
    -- length = length @(LL1 Text) . coerce
