module Block.Class.Search.Class where

import Essentials

import Block.Class.Search.Types (Span (..), Pivot (..))
import Block.Class.End (End (..))
import Block.Class.Item (Item)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)

import qualified Data.List.NonEmpty as NonEmpty

class Search xs where

    {-| Separate a block into two parts by specifying a predicate
        for all the items in the first part

    Where possible, the result is @('SpanPart' spanned remainder)@,
    where @spanned@ is the longest contiguous series starting from the
    designated 'End' of items that match the predicate and @remainder@
    is everything else. If not even the first item matches, the result
    is 'SpanNone'. If all the items match, the result is 'SpanAll'. -}
    span ::
        End -- ^ Which end to start searching from: 'Front' or 'Back'
        -> (Item xs -> Bool)
            -- ^ Keep taking as long as items match this predicate
        -> xs -- ^ A block
        -> Span xs

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

    find = \case
        Front -> \f ->
          let
            go (x :| zs) = case f x of
              Just y -> Just (Pivot Nothing y (nonEmpty zs))
              Nothing -> case nonEmpty zs of
                  Nothing -> Nothing
                  Just zs' -> go zs' <&> \(Pivot a s b) ->
                      Pivot (Just (x :| maybe [] NonEmpty.toList a)) s b
          in
            go
        Back -> \f xs ->
            find Front f (NonEmpty.reverse xs)
            & fmap (fmap NonEmpty.reverse)
