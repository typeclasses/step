module Block.Class.Singleton
  (
    {- * Class -} Singleton (..),
    {- * Superclasses -} Concat (..),
    {- * Types -} Pop (..), End (..),
    {- * Utilities -} unpop, head, pushMaybe, prepend,
  )
  where

import Block.Class.Concat.Class
import Block.Class.Concat.Utilities
import Block.Class.Singleton.Class
import Block.Class.Singleton.Types
import Block.Class.Singleton.Utilities
