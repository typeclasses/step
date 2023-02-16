module Block.Class.Block.Class where

import Block.Class.NonEmptyIso (NonEmptyIso)
import Block.Class.Positional.Class (Positional)
import Block.Class.Search.Class (Search)
import Data.List.NonEmpty (NonEmpty)

class (Positional x xs, Search x xs, NonEmptyIso x xs) => Block x xs | xs -> x

instance Block x (NonEmpty x)
