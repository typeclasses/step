module Cursor.Advancement.Type
  (
    {- * Type -} Advancement (..),
  )
  where

import Essentials

import Block (Shortfall)

data Advancement a b =
    YouCanNotAdvance Shortfall a
  | AdvanceSuccess b
  deriving stock (Eq, Ord, Show)
