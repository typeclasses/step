module Cursor.Advancement.Type
  (
    {- * Type -} Advancement (..),
  )
  where

import Block (Shortfall)

data Advancement =
    AdvanceSuccess
  | YouCanNotAdvance Shortfall
