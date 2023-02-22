module Block.Class.Positional
  (
    {- * Class -} Positional (..),
    {- * Superclasses -} Concat (..),
    {- * Types -} Take (..), Shortfall (..), End (..),
    {- * Utilities -} append,
  )
  where

import Block.Class.Concat.Class
import Block.Class.Concat.Utilities
import Block.Class.End
import Block.Class.Positional.Class
import Block.Class.Positional.Types
