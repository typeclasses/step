module Block.Class.Positional
  (
    {- * Class -} Positional (..),
    {- * Types -} Take (..), BiPrefix (..), WhichOfTwo (..),
    {- * Utilities -} biPrefix, sameItemsTake,
    {- * Re-exported modules -}
            module Block.Class.End,
            module Block.Class.ItemEquivalence,
            module Block.Class.Shortfall,
  )
  where

import Block.Class.Positional.Class
import Block.Class.Positional.Types
import Block.Class.Positional.Utilities

import Block.Class.End
import Block.Class.Shortfall
import Block.Class.ItemEquivalence
