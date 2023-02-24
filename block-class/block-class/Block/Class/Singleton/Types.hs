module Block.Class.Singleton.Types
  (
    {- * Types -} Pop (..),
  )
  where

import Essentials

data Pop x xs = Pop{ item :: x, popRemainder :: Maybe xs }
    deriving stock (Eq, Ord, Show)
