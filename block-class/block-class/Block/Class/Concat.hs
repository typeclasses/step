module Block.Class.Concat
  (
    {- * Class -} Concat (..),
    {- * Types -} End (..),
    {- * Utilities -} append, concatRecursively,
    {- * Re-exported modules -}
            module Block.Class.End,
            module Block.Class.ItemEquality,
  )
  where

import Block.Class.Concat.Class
import Block.Class.Concat.Utilities
import Block.Class.End
import Block.Class.ItemEquality
