{-# language DerivingStrategies, GeneralizedNewtypeDeriving #-}

module Step.Input.CursorPosition where

import Step.Internal.Prelude

newtype CursorPosition = CursorPosition Natural
    deriving newtype (Eq, Ord, Show, Num)

origin :: CursorPosition
origin = 0

absoluteDifference :: CursorPosition -> CursorPosition -> Natural
absoluteDifference (CursorPosition x) (CursorPosition y) = if x > y then x - y else y - x

increase :: Natural -> CursorPosition -> CursorPosition
increase x (CursorPosition p) = CursorPosition (p + x)
