module Block.Class.Block.Class
  (
    {- * Class -} Block,
  )
  where

import Block.Class.Index.Class
import Block.Class.NonEmptyIso.Class
import Block.Class.Search.Class

import Data.List.NonEmpty (NonEmpty)

class (NonEmptyIso x xs, Search x xs, Index x xs) => Block x xs

instance Block x (NonEmpty x)
