{-# language Unsafe #-}

module Positive.Unsafe where

import Numeric.Natural (Natural)

newtype Positive = PositiveNaturalUnsafe{ positiveNatural :: Natural }
