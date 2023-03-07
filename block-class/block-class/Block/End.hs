module Block.End
  (
    {- * Type -} End (..),
    {- * Utilities -} opposite,
  )
  where

import Essentials

data End = Front | Back
  deriving stock (Eq, Ord, Show, Enum, Bounded)

opposite :: End -> End
opposite = \case Front -> Back; Back -> Front
