{-# language Unsafe #-}

module Integer.Positive.Unsafe
  (
    {- * Type -} Positive (FromNatural),
    {- * Conversion -}
    {- ** Natural -} toNatural, fromNatural, fromNaturalChecked,
    {- ** Integer -} toInteger, fromInteger, fromIntegerChecked,
    {- ** Int -} toInt, fromInt, fromIntChecked,
    {- * Arithmetic -} add, subtract, subtractChecked, multiply,
    {- * One (1) -} one, addOne, subtractOne, subtractOneChecked,
  )
  where

import Numeric.Natural (Natural)

import Data.Function (($), (.), const, id)
import Prelude (Eq, Ord, Num, Integral, Real, Integer, Enum, Show, Int)

import qualified Data.Ord as Ord
import qualified Text.Show as Show
import qualified Control.Exception as Exception
import qualified Data.Maybe as Maybe
import qualified Data.Bits as Bits
import qualified Data.List as List
import qualified Prelude as Enum (Enum (..))
import qualified Prelude as Num (Num (..), Integral (..), Real (..), fromIntegral)

newtype Positive = FromNatural{ toNatural :: Natural } deriving (Eq, Ord)

fromNatural :: Natural -> Positive
fromNatural = FromNatural

fromNaturalChecked :: Natural -> Positive
fromNaturalChecked x = case x of 0 -> Exception.throw Exception.Underflow; _ -> fromNatural x

toInteger :: Positive -> Integer
toInteger = Num.toInteger . toNatural

fromInteger :: Integer -> Positive
fromInteger = fromNatural . Num.fromInteger

fromIntegerChecked :: Integer -> Positive
fromIntegerChecked x = case Num.signum x of 1 -> fromInteger x; _ -> Exception.throw Exception.Underflow

add :: Positive -> Positive -> Positive
add a b = fromNatural (toNatural a Num.+ toNatural b)

subtract :: Positive -> Positive -> Positive
subtract a b = fromNatural (toNatural a Num.- toNatural b)

subtractChecked :: Positive -> Positive -> Positive
subtractChecked a b = if a Ord.> b then subtract a b else Exception.throw Exception.Underflow

multiply :: Positive -> Positive -> Positive
multiply a b = fromNatural (toNatural a Num.* toNatural b)

one :: Positive
one = fromNatural 1

addOne :: Positive -> Positive
addOne = fromNatural . Enum.succ . toNatural

subtractOne :: Positive -> Positive
subtractOne = fromNatural . Enum.pred . toNatural

subtractOneChecked :: Positive -> Positive
subtractOneChecked x = case x of { 1 -> Exception.throw Exception.Underflow; _ -> subtractOne x }

toInt :: Positive -> Int
toInt = Num.fromIntegral . toNatural

toIntChecked :: Positive -> Int
toIntChecked = Maybe.fromMaybe (Exception.throw Exception.Overflow) . Bits.toIntegralSized . toNatural

fromInt :: Int -> Positive
fromInt = fromNatural . Num.fromIntegral

fromIntChecked :: Int -> Positive
fromIntChecked x = case Num.signum x of { 1 -> fromInt x; _ -> Exception.throw Exception.Underflow }

enumFrom :: Positive -> [Positive]
enumFrom = List.map fromNatural . Enum.enumFrom . toNatural

enumFromTo :: Positive -> Positive -> [Positive]
enumFromTo a b = List.map fromNatural $ Enum.enumFromTo (toNatural a) (toNatural b)

enumFromThen :: Positive -> Positive -> [Positive]
enumFromThen a b = if b Ord.> a then descending else ascending
  where
    ascending = List.map fromNatural $ Enum.enumFromThen (toNatural a) (toNatural b)
    descending = List.map fromInteger $ List.takeWhile (Ord.>= 1) $
        Enum.enumFromThen (toInteger a) (toInteger b)

enumFromThenTo :: Positive -> Positive -> Positive -> [Positive]
enumFromThenTo a b c = if b Ord.> a then descending else ascending
  where
    ascending = List.map fromNatural $ Enum.enumFromThenTo (toNatural a) (toNatural b) (toNatural c)
    descending = List.map fromInteger $ List.takeWhile (Ord.>= 1) $
        Enum.enumFromThenTo (toInteger a) (toInteger b) (toInteger c)

instance Num Positive
  where
    abs = id
    negate = const (Exception.throw Exception.Underflow)
    signum = const (fromNatural 1)
    fromInteger = fromIntegerChecked
    (+) = add
    (*) = multiply
    (-) = subtractChecked

instance Enum Positive
  where
    succ = addOne
    pred = subtractOneChecked

    fromEnum = toIntChecked
    toEnum = fromIntChecked

    enumFrom = enumFrom
    enumFromTo = enumFromTo
    enumFromThen = enumFromThen
    enumFromThenTo = enumFromThenTo

instance Real Positive
  where
    toRational = Num.toRational . toInteger

instance Integral Positive
  where
    toInteger = toInteger

    quotRem a b =
        let (q, r) = Num.quotRem (toNatural a) (toNatural b)
        in (fromNaturalChecked q, fromNaturalChecked r)

    divMod a b =
        let (d, m) = Num.divMod (toNatural a) (toNatural b)
        in (fromNaturalChecked d, fromNaturalChecked m)

instance Show Positive
  where
    show = Show.show . toNatural
    showsPrec i = Show.showsPrec i . toNatural
