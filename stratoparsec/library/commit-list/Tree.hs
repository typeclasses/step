module Tree where

data Tree k a =
  Tree k
    (Maybe (Tree k a)) -- ^ Actively being explored
    [a] -- ^ Alternative possibilities

type Key k = (Ord k)

singleton :: Key k => k -> a -> Tree k a
singleton = _

insert :: Key k =>
    k -- ^ Prepend to the list at this location
    -> k
    -> a
    -> Tree k a
    -> Tree k a
insert = _

delete :: Key k => k -> Tree k a -> Tree k a
delete = _

-- Removes alternative possibilities at a key
prune :: Key k => k -> Tree k a -> Tree k a
prune = _

leftmost :: Key k => Tree k a -> Maybe (k, a, Tree k a)
leftmost = _

simplify :: Key k => Tree k a -> Tree k a
simplify = _
