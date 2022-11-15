{-# language Trustworthy #-}

module Integer.Signed
  (
    {- * Type -} Signed (Zero, NonZero, Plus, Minus),
    {- * Conversion -}
    {- ** Integer -} toInteger, fromInteger,
    {- ** Natural -} toNatural, fromNatural,
    {- ** Positive -} toPositive, fromPositive,
    {- ** Int -} toInt, fromInt,
  )
  where

import Prelude (Eq, Ord, Num, Integral, Real, Integer, Enum, Show)
import Data.Function (($), (.))
import Data.Int (Int)
import Data.Maybe (Maybe (..))
import Integer.Positive.Unsafe (Positive)
import Integer.Sign (Sign (..))
import Numeric.Natural (Natural)

import qualified Integer.Sign as Sign
import qualified Integer.Positive.Unsafe as Positive.Unsafe
import qualified Data.Ord as Ord
import qualified Data.List as List
import qualified Prelude as Enum (Enum (..))
import qualified Prelude as Num (Num (..), Integral (..), Real (..))
import qualified Prelude as Bounded (Bounded (..))
import qualified Text.Show as Show

data Signed = Zero | NonZero Sign Positive
    deriving (Eq, Ord)

pattern Minus :: Positive -> Signed
pattern Minus x = NonZero MinusSign x
pattern Plus :: Positive -> Signed

pattern Plus x = NonZero PlusSign x

{-# complete Zero, Minus, Plus #-}

fromPositive :: Positive -> Signed
fromPositive = Plus

toPositive :: Signed -> Maybe Positive
toPositive (Plus x) = Just x
toPositive _ = Nothing

fromNatural :: Natural -> Signed
fromNatural 0 = Zero
fromNatural x = Plus $ Positive.Unsafe.fromNatural x

toNatural :: Signed -> Maybe Natural
toNatural (Minus _) = Nothing
toNatural Zero = Just 0
toNatural (Plus x) = Just (Positive.Unsafe.toNatural x)

add :: Signed -> Signed -> Signed
add Zero x = x
add x Zero = x
add (NonZero sa a) (NonZero sb b) = case (sa, sb) of
    (PlusSign, PlusSign)   -> Plus  $ a Num.+ b
    (MinusSign, MinusSign) -> Minus $ a Num.+ b

    (MinusSign, PlusSign) -> case Ord.compare a b of
        Ord.EQ -> Zero
        Ord.LT -> Plus  $ Positive.Unsafe.subtract b a
        Ord.GT -> Minus $ Positive.Unsafe.subtract a b

    (PlusSign, MinusSign) -> case Ord.compare a b of
        Ord.EQ -> Zero
        Ord.LT -> Minus $ Positive.Unsafe.subtract b a
        Ord.GT -> Plus  $ Positive.Unsafe.subtract a b

negate :: Signed -> Signed
negate Zero = Zero
negate (NonZero s x) = NonZero (Sign.negate s) x

multiply :: Signed -> Signed -> Signed
multiply Zero _ = Zero
multiply _ Zero = Zero
multiply (NonZero sa a) (NonZero sb b) =
    NonZero (Sign.multiply sa sb) (a Num.* b)

abs :: Signed -> Signed
abs Zero = Zero
abs x@(NonZero s p) = case s of
    PlusSign -> x
    MinusSign -> NonZero PlusSign p

signum :: Signed -> Signed
signum Zero = Zero
signum (NonZero s _) = NonZero s Positive.Unsafe.one

fromInteger :: Integer -> Signed
fromInteger x = case Ord.compare x 0 of
    Ord.EQ -> Zero
    Ord.LT -> Minus $ Positive.Unsafe.fromInteger $ Num.abs x
    Ord.GT -> Plus  $ Positive.Unsafe.fromInteger x

toInteger :: Signed -> Integer
toInteger Zero = 0
toInteger (Plus x) = Positive.Unsafe.toInteger x
toInteger (Minus x) = Num.negate $ Positive.Unsafe.toInteger x

toInt :: Signed -> Maybe Int
toInt x = case x of
    Zero -> Just 0
    Plus p -> if ok then Just (Num.fromInteger i) else Nothing
      where
        ok = i Ord.<= Num.toInteger (Bounded.maxBound :: Int)
        i = Positive.Unsafe.toInteger p
    Minus p -> if ok then Just (Num.fromInteger i) else Nothing
      where
        ok = i Ord.>= Num.toInteger (Bounded.minBound :: Int)
        i = Num.negate (Positive.Unsafe.toInteger p)

fromInt :: Int -> Signed
fromInt x = case Ord.compare x 0 of
    Ord.EQ -> Zero
    Ord.GT -> Plus $ Positive.Unsafe.fromInt x
    Ord.LT -> Minus $ Positive.Unsafe.fromInteger $ Num.negate $ Num.toInteger x

instance Num Signed
  where
    (+) = add
    (*) = multiply
    negate = negate
    abs = abs
    signum = signum
    fromInteger = fromInteger

instance Enum Signed
  where
    pred = fromInteger . Enum.pred . toInteger
    succ = fromInteger . Enum.succ . toInteger

    toEnum = fromInteger . Enum.toEnum
    fromEnum = Enum.fromEnum . toInteger

    enumFrom a = List.map fromInteger $ Enum.enumFrom (toInteger a)
    enumFromTo a b = List.map fromInteger $ Enum.enumFromTo (toInteger a) (toInteger b)
    enumFromThen a b = List.map fromInteger $ Enum.enumFromThen (toInteger a) (toInteger b)
    enumFromThenTo a b c = List.map fromInteger $ Enum.enumFromThenTo (toInteger a) (toInteger b) (toInteger c)

instance Real Signed
  where
    toRational = Num.toRational . toInteger

instance Integral Signed
  where
    toInteger = toInteger

    quotRem a b =
        let (q, r) = Num.quotRem (toInteger a) (toInteger b)
        in (fromInteger q, fromInteger r)

    divMod a b =
        let (d, m) = Num.quotRem (toInteger a) (toInteger b)
        in (fromInteger d, fromInteger m)

instance Show Signed
  where
    show = Show.show . Num.toInteger
    showsPrec i = Show.showsPrec i . Num.toInteger
