module Block.Class.Singleton.Types
  (
    Pop (..), End (..),
  )
  where

import Essentials

import Block.Class.End (End (..))

data Pop x xs = Pop{ item :: x, remainder :: Maybe xs }
    deriving stock (Eq, Ord, Show)
