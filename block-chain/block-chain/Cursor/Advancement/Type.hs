module Cursor.Advancement.Type
  (
    {- * Type -} Advancement (..),
  )
  where

import Integer (Positive)

data Advancement =
    AdvanceSuccess
  | YouCanNotAdvance{ shortfall :: Positive }
