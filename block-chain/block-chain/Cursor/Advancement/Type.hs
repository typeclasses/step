module Cursor.Advancement.Type
  (
    {- * Type -} Advancement (..),
  )
  where

import Block (Shortfall)

data Advancement a b =
    YouCanNotAdvance Shortfall a
  | AdvanceSuccess b
