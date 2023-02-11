module Block.Block.Class where

import Block.Positional.Class (Positional)
import Block.Search.Class (Search)
import Data.List.NonEmpty (NonEmpty)

class (Positional xs, Search xs) => Block xs

instance Block (NonEmpty xs)
