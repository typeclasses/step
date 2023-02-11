module Block.Class.Block.Class where

import Block.Class.Positional.Class (Positional)
import Block.Class.Search.Class (Search)
import Data.List.NonEmpty (NonEmpty)

class (Positional xs, Search xs) => Block xs

instance Block (NonEmpty xs)
