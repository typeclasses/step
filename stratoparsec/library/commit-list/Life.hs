module Life where

import Tree (Tree)
import qualified Tree
import Infinity (Infinity (Infinity))

data Life k a = Life{ nextKeys :: Infinity k, tree :: Tree k a }

singleton :: Infinity k -> a -> Life k a
singleton (Infinity k ks) x =
    Life{ nextKeys = ks, tree = Tree.singleton k x }

insert ::
    k -- ^ Insert under this node
    -> a
    -> Life k a
    -> Life k a
insert parent x l =
    l{ nextKeys = ks, tree = l & tree & Tree.insert parent k x }
  where
    Infinity k ks = nextKeys l

delete :: k -> Life k a -> Life k a
delete k l = l{ tree = l & tree & Tree.delete k }

substitute :: k -> a -> Life k a -> Life k a
substitute k x l = l{ tree = l & tree & Tree.substitute k x }

-- @prune a b@ removes all descendants of @a@ other than those that lead to @b@.
prune :: k -> k -> Life k a -> Life k a
prune a b l = l{ tree = l & tree & Tree.prune a b }

leftmost :: Life k a -> Maybe (k, a)
leftmost l = l & tree & Tree.leftmost

simplify :: Life k a -> Life k a
simplify l = l{ tree = l & tree & Tree.simplify }
