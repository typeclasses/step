module Block.Class.Search.Utilities where

import Essentials
import Block.Class.Search.Types
import Block.Class.Search.Class
import Block.Class.State
import Block.Class.End

findPredicate :: Search x xs => End -> (x -> Bool) -> xs -> Maybe (Pivot x xs)
findPredicate end f = stateless . find end (\x -> pure $ if f x then Just x else Nothing)
