module Block.Block.Class where

import Block.Positional.Class (Positional)
import Block.Search.Class (Search)

class (Positional xs, Search xs) => Block xs
