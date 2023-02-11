module Block.Class.Search.Class where

import Essentials

import Block.Class.Search.Types (Span (..), Pivot (..))
import Block.Class.End (End (..))
import Block.Class.Item (Item)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)

import qualified Integer.Positive as Positive
import qualified Data.List.NonEmpty as NonEmpty

class Search xs where

    {-|
    @span Front isUpper "ABCdefGHI" = Span "ABC" "defGHI"@

    @span Back isUpper "ABCdefGHI" = Span "GHI" "ABCdef"@

    @span Back isLetter "ABCdefGHI" = SpanAll@

    @span Back isDigit "ABCdefGHI" = SpanNone@
    -}
    span :: End -> (Item xs -> Bool) -> xs -> Span xs

    {-|
    @find Front (\x -> readMaybe [x]) "abc1def2ghi" = Just (Pivot (Just "abc") 1 (Just "def2ghi"))@

    @find Back (\x -> readMaybe [x]) "abc1def2ghi" = Just (Pivot (Just "abc1def") 2 (Just "ghi"))@

    @find Front (\x -> Just x <* guard isLetter) "abc" = Just (Pivot Nothing 'a' (Just "bc"))@

    @find _ (\_ -> Nothing) _ = Nothing@
    -}
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
