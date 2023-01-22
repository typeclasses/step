module Cursor.Interface
  (
    {- * Types -} Cursor (..), Mode (..), Advancement (..),
            Step (..), CursorRead, CursorWrite,
    {- * Class -} IsCursor (..), commit, reset, next,
  )
  where

import Cursor.Interface.Type
import Cursor.Interface.Class
