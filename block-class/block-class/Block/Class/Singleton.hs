module Block.Class.Singleton
  (
    {- * Class -} Singleton (..),
    {- * Types -} Pop (..),
    {- * Utilities -}
          unpop, first, last, terminal, pushMaybe, sameItemsPop,
    {- * Re-exported modules -}
          module Block.Class.End,
  )
  where

import Block.Class.Singleton.Class
import Block.Class.Singleton.Types
import Block.Class.Singleton.Utilities

import Block.Class.End
