module Cursor.Interface
  (
    {- * Types -} Cursor (..), Step (..),
    {- * Class -} IsCursor (..), next, push, flush,
  )
  where

import Cursor.Interface.Type
import Cursor.Interface.Class
