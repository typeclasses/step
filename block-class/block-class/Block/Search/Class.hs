module Block.Search.Class where

import Essentials

import Block.Search.Types
import Block.End
import Block.Item

class Search xs where

    {-|
    @span Front isUpper "ABCdefGHI" = Span "ABC" "defGHI"@

    @span Back isUpper "ABCdefGHI" = Span "ABCdef" "GHI"@

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
