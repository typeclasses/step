module Block.Text.Type
  (
    {- * Type -} Text1,
  )
  where

import Essentials
import Block.Class

import Data.Char (Char)
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Ord (compare, Ordering (..))
import Prelude (error)
import Data.Int (Int)
import Integer (Signed (..))
import Block.ListLike.Type (LL1)

import Data.ListLike
import GHC.Exts (Item)

import qualified Data.Foldable as Foldable
import qualified Data.Text as Text
import qualified Block.Class as Block
import qualified Integer.Positive as Positive
import qualified Integer

newtype Text1 = Text1 (LL1 Char Text)
    deriving newtype (Eq, Ord, Show, Semigroup, Singleton Char, Search Char)

-- deriving via (LL1 Char Text) instance Singleton Char Text1
-- deriving via (LL1 Char Text) instance Search Char Text1

-- instance Semigroup Text1 where
--     Text1 a <> Text1 b = Text1 (a <> b)

-- instance Refined Char Text Text1 where
--     refine x = if Text.null x then Nothing else Just (Text1 x)
--     generalize (Text1 x) = x
--     assume = Text1

-- type instance Item Text1 = Char

-- type instance Nullable Text1 = Text

-- instance Block Char Text1

-- instance Singleton Text1 where

--     singleton = Text.singleton >>> assume

--     push Front x xs = assume (Text.cons x (generalize xs))
--     push Back x xs = assume (Text.snoc (generalize xs) x)

--     pop Front xs = case Text.uncons (generalize xs) of
--         Just (x, xs') -> Block.Pop x (refine xs')
--         Nothing -> error "Text1 pop: the impossible happened"
--     pop Back xs = case Text.unsnoc (generalize xs) of
--         Just (xs', x) -> Block.Pop x (refine xs')
--         Nothing -> error "Text1 pop: the impossible happened"

-- instance Positional Char Text1 where

    -- length = generalize >>> Text.length >>> Integer.yolo

-- instance Search Char Text1 where

-- instance NonEmptyIso Char Text1 where
    -- toNonEmpty = generalize >>> Text.unpack >>> assume
    -- fromNonEmpty = Foldable.toList >>> Text.pack >>> assume

    -- leftView = (\(Pop i r) -> Pop i (coerce r)) . leftView @(LL1 Text) . coerce
    -- leftReview = coerce . leftReview @(LL1 Text) . (\(Pop i r) -> Pop i (coerce r))
    -- span p = fmap coerce . span @(LL1 Text) p . coerce
    -- divide f = fmap coerce . divide @(LL1 Text) f . coerce
    -- split n = fmap coerce . split @(LL1 Text) n . coerce
    -- take n = fmap coerce . take @(LL1 Text) n . coerce
    -- drop n = fmap coerce . drop @(LL1 Text) n . coerce
    -- while p = fmap coerce . while @(LL1 Text) p . coerce
    -- length = length @(LL1 Text) . coerce
