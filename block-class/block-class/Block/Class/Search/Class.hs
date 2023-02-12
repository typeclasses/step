module Block.Class.Search.Class where

import Essentials

import Block.Class.Search.Types (Span (..), Pivot (..))
import Block.Class.End (End (..))
import Block.Class.Item (Item)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)

import qualified Integer.Positive as Positive
import qualified Data.List.NonEmpty as NonEmpty

class Search xs where

    span :: End -> (Item xs -> Bool) -> xs -> Span xs

    find :: End -> (Item xs -> Maybe p) -> xs -> Maybe (Pivot p xs)

instance Search (NonEmpty xs) where

    span = \case
        Front -> \f xs -> let (a, b) = NonEmpty.span f xs in
            case (nonEmpty a, nonEmpty b) of
                (Nothing, _) -> SpanNone
                (_, Nothing) -> SpanAll
                (Just a', Just b') -> SpanPart a' b'
        Back -> \f xs -> span Front f (NonEmpty.reverse xs) &
            \case
                SpanNone -> SpanNone
                SpanAll -> SpanAll
                SpanPart a b -> SpanPart (NonEmpty.reverse a) (NonEmpty.reverse b)
