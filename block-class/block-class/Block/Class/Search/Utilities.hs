module Block.Class.Search.Utilities
  (
    {- * Utilities -} findPredicate, spanPredicate, sameItemsSpan,
  )
  where

import Essentials

import Block.Class.End (End)
import Block.Class.ItemEquality.Class (ItemEquality (..))
import Block.Class.Search.Class (Search (..))
import Block.Class.Search.Types (Span (..), Pivot)
import Block.Class.State.Utilities (stateless)
import Data.Bool ((&&))

findPredicate :: Search x xs => End -> (x -> Bool) -> xs -> Maybe (Pivot x xs)
findPredicate end f =
    stateless . find end (\x -> pure $ if f x then Just x else Nothing)

spanPredicate :: Search x xs => End -> (x -> Bool) -> xs -> Span xs
spanPredicate end f = stateless . span end (pure . f)

sameItemsSpan :: ItemEquality xs => Span xs -> Span xs -> Bool
sameItemsSpan = \case
    SpanNone -> \case SpanNone -> True; _ -> False
    SpanAll -> \case SpanAll -> True; _ -> False
    SpanPart a1 b1 -> \case
        SpanPart a2 b2 -> sameItems a1 a2 && sameItems b1 b2
        _ -> False
