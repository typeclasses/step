module Block.Class.Search.Utilities where

import Block.Class.Search.Types (Pivot (..))
import Block.Class.Singleton.Class (Singleton (..))
import Block.Class.End (End (..))
import Control.Applicative (liftA2)

pivotItemJoin :: (Singleton x xs) =>
    Pivot (Pivot p x) xs -> Pivot p xs
pivotItemJoin (Pivot xs1 (Pivot x1 p x2) xs2) =
    Pivot (liftA2 (push Back) x1 xs1) p (liftA2 (push Front) x2 xs2)
