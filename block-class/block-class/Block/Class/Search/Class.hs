module Block.Class.Search.Class where

import Essentials

import Block.Class.Search.Types (Span (..), Pivot (..))
import Block.Class.End (End (..))
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Function (fix)
import Block.Class.State (State (..))

import qualified Data.List.NonEmpty as NonEmpty

class Search x xs | xs -> x where

    {-| Separate a block into two parts by specifying a predicate
        for all the items in the first part

    Where possible, the result is @('SpanPart' spanned remainder)@,
    where @spanned@ is the longest contiguous series starting from the
    designated 'End' of items that match the predicate and @remainder@
    is everything else. If not even the first item matches, the result
    is 'SpanNone'. If all the items match, the result is 'SpanAll'. -}
    span ::
        End -- ^ Which end to start searching from: 'Front' or 'Back'
        -> (x -> State s Bool)
            -- ^ Keep taking as long as items match this predicate
        -> xs -- ^ A block
        -> State s (Span xs)

    {-| Search a block for the first item for which the given function
        returns @Just@

    If a match is found, the result is @(Just ('Pivot' a found b))@,
    which represents a division of the block into three parts. Part
    @(a :: Maybe xs)@ consists of the items that the search examined
    before finding a match. The @found@ value is the result obtained from
    the matching item. Part @(b :: Maybe xs)@ consists of the items that
    the search did not examine.

    If no match is found, the result is @Nothing@. -}
    find ::
        End  -- ^ Which end to start searching from: 'Front' or 'Back'
        -> (x -> State s (Maybe found))
            -- ^ Specification of what kind of item this search is looking
            --   for; when this function returns @Just@, an item is found
            --   and the search is over.
        -> xs -- ^ A block
        -> State s (Maybe (Pivot found xs))

instance Search x (NonEmpty x) where

    span ::
        End -> (x -> State s Bool) -> NonEmpty x -> State s (Span (NonEmpty x))

    span Front f = fix \r (x :| xs) -> f x >>= \case
        False -> pure SpanNone
        True -> case xs of
            [] -> pure SpanAll
            y : ys -> r (y :| ys) <&> \case
                SpanAll        ->  SpanAll
                SpanNone       ->  SpanPart (x :| []) (y :| ys)
                SpanPart as b  ->  SpanPart (x :| NonEmpty.toList as) b

    span Back f = \xs -> span Front f (NonEmpty.reverse xs) <&> \case
        SpanNone      ->  SpanNone
        SpanAll       ->  SpanAll
        SpanPart a b  ->  SpanPart (NonEmpty.reverse a) (NonEmpty.reverse b)

    find :: End -> (x -> State s (Maybe found)) -> NonEmpty x
        -> State s (Maybe (Pivot found (NonEmpty x)))

    find Front f = fix \r (x :| xs) -> f x >>= \case
        Just found -> pure $ Just $ Pivot Nothing found (nonEmpty xs)
        Nothing -> case xs of
            [] -> pure Nothing
            y : ys -> r (y :| ys) <&> fmap \(Pivot asm found bs) ->
                Pivot (Just (x :| maybe [] NonEmpty.toList asm)) found bs

    find Back f = \xs ->
        find Front f (NonEmpty.reverse xs) <&> fmap (fmap NonEmpty.reverse)
