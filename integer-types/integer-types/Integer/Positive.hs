{-# language Trustworthy #-}

module Integer.Positive
  (
    {- * Type -} Positive,
    {- * Arithmetic -} add, subtract, multiply,
    {- * Conversion -}
    {- ** Natural -} toNatural, fromNatural,
    {- ** Integer -} toInteger, fromInteger,
    {- ** Signed -} toSigned, fromSigned,
    {- * One (1) -} one, addOne, subtractOne,
  )
  where

import Data.Function (($))
import Data.Maybe (Maybe (..))
import Integer.Positive.Unsafe (Positive, toNatural, one, add, addOne, multiply, toInteger)
import Integer.Signed (Signed (..))
import Numeric.Natural (Natural)
import Prelude (Integer)

import qualified Prelude as Num (Num (..))
import qualified Data.Ord as Ord
import qualified Integer.Positive.Unsafe as Unsafe

fromInteger :: Integer -> Maybe Positive
fromInteger x = if x Ord.> 0 then Just (Unsafe.fromInteger x) else Nothing

fromNatural :: Natural -> Maybe Positive
fromNatural x = case x of 0 -> Nothing; _ -> Just (Unsafe.fromNatural x)

subtract :: Positive -> Positive -> Signed
subtract a b = case Ord.compare a b of
    Ord.EQ -> Zero
    Ord.GT -> Plus  $ Unsafe.subtract a b
    Ord.LT -> Minus $ Unsafe.subtract b a

subtractOne :: Positive -> Natural
subtractOne x = toNatural x Num.- 1

toSigned :: Positive -> Signed
toSigned = Plus

fromSigned :: Signed -> Maybe Positive
fromSigned (Plus x) = Just x
fromSigned _ = Nothing
