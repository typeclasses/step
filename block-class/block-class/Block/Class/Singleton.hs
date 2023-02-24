module Block.Class.Singleton
  (
    {- * Class -} Singleton (..),
    {- * Types -} Pop (..),
    {- * Utilities -} unpop, first, last, terminal, pushMaybe,
    {- * Re-exported modules -} module Block.Class.Concat,
  )
  where

import Block.Class.Concat
import Block.Class.Singleton.Class
import Block.Class.Singleton.Types
import Block.Class.Singleton.Utilities
