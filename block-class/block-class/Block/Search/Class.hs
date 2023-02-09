module Block.Search.Class where

import Essentials

import Block.Search.Types
import Block.End
import Block.Item

class Search xs where

    span :: End -> (Item xs -> Bool) -> xs -> Span xs

    find :: End -> (Item xs -> Maybe p) -> xs -> Maybe (Pivot p xs)
