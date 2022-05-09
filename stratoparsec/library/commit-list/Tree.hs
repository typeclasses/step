module Tree where

{-

- Each node has a unique key.
- A node is either:
  - a branch (key, and any number of children)
  - a leaf (key and value)

-}

data Tree k a

type Key k = (Ord k)

empty :: Tree k a
empty = _

singleton :: k -> a -> Tree k a
singleton = _

insert ::
    k -- ^ Insert under this node
    -> k
    -> a
    -> Tree k a
    -> Tree k a
insert = _

-- @prune a b@ removes all descendants of @a@ other than those that lead to @b@.
prune :: k -> k -> Tree k a -> Tree k a
prune = _

depthFirstView :: Tree k a -> Maybe (k, a, Tree k a)
depthFirstView = _

simplify :: Tree k a -> Tree k a
simplify = _
