module Cursor.Interface
  (
    {- * Types -} Cursor (..), Mode (..), Advancement (..),
            Step (..), CursorRead, CursorWrite,
    {- * Class -} IsCursor (..), commit, reset, next,
    {- * Utilities -} commitAlternative,
  )
  where

import Cursor.Interface.Type
import Cursor.Interface.Class
import Cursor.Interface.Utilities
