module Reset.Interface
  (
    {- * Types -} Reset (..), Step (..),
    {- * Class -} ResetStream (..),
    {- * Requests -} next, reset,
  )
  where

import Reset.Interface.Type
import Reset.Interface.Class
