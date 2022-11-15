{-# language Safe #-}

module Integer.Integer
  (
    {- * Type -} Integer,
    {- * Conversion -}
    {- ** Positive -} toPositive, fromPositive,
    {- ** Natural -} toNatural, fromNatural,
    {- ** Signed -} toSigned, fromSigned,
    {- ** Int -} toInt, fromInt,
  )
  where

import Data.Maybe (Maybe (..))
import Integer.Positive (Positive)
import Integer.Signed (Signed (..))
import Numeric.Natural (Natural)
import Prelude (Integer)
import Data.Int (Int)

import qualified Data.Bool as Bool
import qualified Data.Ord as Ord
import qualified Integer.Natural as Natural
import qualified Integer.Positive as Positive
import qualified Integer.Signed as Signed
import qualified Prelude as Bounded (Bounded (..))
import qualified Prelude as Num (Num (..), Integral (..))

toPositive :: Integer -> Maybe Positive
toPositive = Positive.fromInteger

fromPositive :: Positive -> Integer
fromPositive = Positive.toInteger

toNatural :: Integer -> Maybe Natural
toNatural = Natural.fromInteger

fromNatural :: Natural -> Integer
fromNatural = Natural.toInteger

toSigned :: Integer -> Signed
toSigned = Signed.fromInteger

fromSigned :: Signed -> Integer
fromSigned = Signed.toInteger

toInt :: Integer -> Maybe Int
toInt x = if ok then Just (Num.fromInteger x) else Nothing
  where
    ok = x Ord.>= Num.toInteger (Bounded.minBound :: Int) Bool.&&
        x Ord.<= Num.toInteger (Bounded.maxBound :: Int)

fromInt :: Int -> Integer
fromInt = Num.toInteger
