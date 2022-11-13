{-# language Trustworthy #-}

module Integer.Positive
  (
    Positive,
    toNatural, fromNatural,
    one, addOne, subtractOne,
    add, subtract,
  )
  where

import Data.Maybe (Maybe (..))
import Integer.Positive.Unsafe (Positive, toNatural, one, add, addOne)
import Numeric.Natural (Natural)
import Integer.Signed (Signed (..))
import Data.Function (($))

import qualified Prelude as Num (Num (..))
import qualified Data.Ord as Ord
import qualified Integer.Positive.Unsafe as Unsafe

fromNatural :: Natural -> Maybe Positive
fromNatural x = case x of 0 -> Nothing; _ -> Just (Unsafe.fromNatural x)

subtract :: Positive -> Positive -> Signed
subtract a b = case Ord.compare a b of
    Ord.EQ -> Zero
    Ord.GT -> Plus  $ Unsafe.subtract a b
    Ord.LT -> Minus $ Unsafe.subtract b a

subtractOne :: Positive -> Natural
subtractOne x = toNatural x Num.- 1
