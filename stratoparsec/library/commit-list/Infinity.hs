module Infinity where

data Infinity a = Infinity a (Infinity a)
    deriving stock Functor

natural :: Infinity Natural
natural = go 0
  where
    go x = Infinity x (go (x + 1))
