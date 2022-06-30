module Step.CursorPosition.Base where

import Step.Internal.Prelude

newtype CursorPosition = CursorPosition Natural
    deriving newtype (Eq, Ord, Show, Num)

absoluteDifference :: CursorPosition -> CursorPosition -> Natural
absoluteDifference (CursorPosition x) (CursorPosition y) = if x > y then x - y else y - x
