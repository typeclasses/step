module Block.Class.Shortfall where

import Essentials

import Integer (Positive)
import Prelude (Num, Real, Integral)

{-| @(Shortfall n)@ indicates that an operation which failed
    would require a block operand to have @(n)@ more items to
    succeed. -}
newtype Shortfall = Shortfall Positive
    deriving stock (Eq, Ord, Show)
    deriving newtype (Num, Real, Integral, Enum)
