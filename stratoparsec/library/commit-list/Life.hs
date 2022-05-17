module Life where

import Tree (Tree, Key)
import qualified Tree
import Infinity (Infinity (Infinity))

data Life k a = Life{ nextKeys :: Infinity k, tree :: Tree k a }

singleton :: Key k => Infinity k -> a -> Life k a
singleton (Infinity k ks) x =
    Life{ nextKeys = ks, tree = Tree.singleton k x }

insert :: Key k =>
    k -- ^ Prepend to the list at this location
    -> a
    -> Life k a
    -> Life k a
insert parent x l@(nextKeys -> Infinity k ks) =
    l{ nextKeys = ks, tree = l & tree & Tree.insert parent k x }

delete :: Key k => k -> Life k a -> Life k a
delete k l = l{ tree = l & tree & Tree.delete k }

-- Removes alternative possibilities at a key
prune :: Key k => k -> Life k a -> Life k a
prune k l = l{ tree = l & tree & Tree.prune k }

leftmost :: Key k => Life k a -> Maybe (k, a, Life k a)
leftmost l = l & tree & Tree.leftmost <&> \(k, a, t) -> (k, a, l{ tree = t })

simplify :: Key k => Life k a -> Life k a
simplify l = l{ tree = l & tree & Tree.simplify }
