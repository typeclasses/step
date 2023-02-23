module Block.Class.Search
  (
    {- * Class -} Search (..),
    {- * Superclasses -} Singleton (..), Concat (..),
    {- * Types -} Pivot (..), Span (..), End (..), State (..),
    {- * Utilities -} unpop, first, last, terminal, pushMaybe, append, findPredicate,
    {- ** State -} runState, evalState, execState, stateless, get, put, modify,
  )
  where

import Block.Class.Concat.Class
import Block.Class.Concat.Utilities
import Block.Class.End
import Block.Class.Search.Class
import Block.Class.Search.Types
import Block.Class.Search.Utilities
import Block.Class.Singleton.Class
import Block.Class.Singleton.Utilities
import Block.Class.State.Types
import Block.Class.State.Utilities
