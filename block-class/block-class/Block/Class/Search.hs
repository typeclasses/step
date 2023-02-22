module Block.Class.Search
  (
    {- * Class -} Search (..),
    {- * Superclasses -} Concat (..),
    {- * Types -} Pivot (..), Span (..), End (..),
    {- * Utilities -} append,
  )
  where

import Block.Class.Concat.Class
import Block.Class.Concat.Utilities
import Block.Class.End
import Block.Class.Search.Class
import Block.Class.Search.Types
