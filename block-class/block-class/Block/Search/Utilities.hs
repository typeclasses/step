module Block.Search.Utilities where

import Block.Search.Types
import Block.Item
import Block.Singleton
import Block.End

pivotItemJoin :: (Singleton xs, Item xs ~ x) => Pivot (Pivot p x) xs -> Pivot p xs
pivotItemJoin (Pivot xs1 (Pivot x1 p x2) xs2) =
    Pivot (pushMaybes Back x1 xs1) p (pushMaybes Front x2 xs2)
