module Block.Class.Block.Class
  (
    {- * Class -} Block,
  )
  where

import Essentials

import Block.Class.Concat.Class
import Block.Class.Enumerate.Class
import Block.Class.Index.Class
import Block.Class.ItemEquality.Class
import Block.Class.Construct.Class
import Block.Class.Search.Class

import Data.List.NonEmpty (NonEmpty)

class
  (
    Concat xs,
    Construct x xs,
    Enumerate x xs,
    Search x xs,
    Index x xs,
    ItemEquality xs
  ) =>
    Block x xs

instance (Eq x) => Block x (NonEmpty x)
