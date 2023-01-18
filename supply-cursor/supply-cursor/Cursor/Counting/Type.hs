module Cursor.Counting.Type
  (
    {- * Type -} Counting (..),
  )
  where

import Cursor.Interface.Type (Cursor, Mode (..))
import Numeric.Natural (Natural)

data Counting block product =
    Order (Cursor 'Write block product)
  | (product ~ Natural) => AmountCommitted
