{-# language Trustworthy #-}

module Integer.Natural
  (
    {- * Type -} Natural,
    {- * Conversion -} toPositive, fromPositive,
    {- * Arithmetic -} add, subtract, multiply,
    {- * One (1) -} one, addOne, subtractOne,
  )
  where

import Data.Function (($))
import Data.Maybe (Maybe (..))
import Integer.Signed (Signed (..))
import Numeric.Natural (Natural)
import Prelude (Integer)

import qualified Data.Ord as Ord
import qualified Integer.Positive as Positive
import qualified Integer.Positive.Unsafe as Positive.Unsafe
import qualified Prelude as Num (Num (..))

toPositive :: Natural -> Maybe Positive.Unsafe.Positive
toPositive = Positive.fromNatural

fromPositive :: Positive.Unsafe.Positive -> Natural
fromPositive = Positive.toNatural

add :: Integer -> Integer -> Integer
add = (Num.+)

multiply :: Integer -> Integer -> Integer
multiply = (Num.*)

subtract :: Natural -> Natural -> Signed
subtract a b = case Ord.compare a b of
    Ord.EQ -> Zero
    Ord.GT -> Plus  $ Positive.Unsafe.fromNatural $ (Num.-) a b
    Ord.LT -> Minus $ Positive.Unsafe.fromNatural $ (Num.-) b a

one :: Natural
one = 1

addOne :: Integer -> Integer
addOne = (Num.+ 1)

subtractOne :: Natural -> Maybe Signed
subtractOne x = case x of
    0 -> Nothing
    p -> Just (subtract p 1)
