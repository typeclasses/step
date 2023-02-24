module Block.Class.Search
  (
    {- * Class -} Search (..),
    {- * Types -} Pivot (..), Span (..),
    {- * Utilities -} findPredicate, spanPredicate,
    {- * Re-exported modules -}
            module Block.Class.Singleton,
            module Block.Class.State,
  )
  where

import Block.Class.Search.Class
import Block.Class.Search.Types
import Block.Class.Search.Utilities
import Block.Class.Singleton
import Block.Class.State
