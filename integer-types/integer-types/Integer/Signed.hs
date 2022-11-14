{-# language Trustworthy #-}

module Integer.Signed
  (
    {- * Type -} Signed (Zero, NonZero, Plus, Minus),
    {- * Conversion -} fromInteger, toInteger,
    {- * Arithmetic -} add, negate, multiply, abs, signum,
  )
  where

import Prelude (Eq, Num, Integral, Real, Integer, Enum, Show)
import Data.Function (($), (.))
import Data.Ord (Ord (..), Ordering (..))
import Integer.Positive.Unsafe (Positive)
import Integer.Sign (Sign (..))

import qualified Integer.Sign as Sign
import qualified Integer.Positive.Unsafe as Positive
import qualified Text.Show as Show
import qualified Data.List as List
import qualified Prelude as Enum (Enum (..))
import qualified Prelude as Num (Num (..), Integral (..), Real (..))

data Signed = Zero | NonZero Sign Positive
    deriving (Eq, Ord)

pattern Minus :: Positive -> Signed
pattern Minus x = NonZero MinusSign x
pattern Plus :: Positive -> Signed

pattern Plus x = NonZero PlusSign x

{-# complete Zero, Minus, Plus #-}

add :: Signed -> Signed -> Signed
add Zero x = x
add x Zero = x
add (NonZero sa a) (NonZero sb b) = case (sa, sb) of
    (PlusSign, PlusSign)   -> Plus  $ Positive.add a b
    (MinusSign, MinusSign) -> Minus $ Positive.add a b

    (MinusSign, PlusSign) -> case compare a b of
        EQ -> Zero
        LT -> Plus  $ Positive.subtract b a
        GT -> Minus $ Positive.subtract a b

    (PlusSign, MinusSign) -> case compare a b of
        EQ -> Zero
        LT -> Minus $ Positive.subtract b a
        GT -> Plus  $ Positive.subtract a b

negate :: Signed -> Signed
negate Zero = Zero
negate (NonZero s x) = NonZero (Sign.negate s) x

multiply :: Signed -> Signed -> Signed
multiply Zero _ = Zero
multiply _ Zero = Zero
multiply (NonZero sa a) (NonZero sb b) =
    NonZero (Sign.multiply sa sb) (Positive.multiply a b)

abs :: Signed -> Signed
abs Zero = Zero
abs x@(NonZero s p) = case s of
    PlusSign -> x
    MinusSign -> NonZero PlusSign p

signum :: Signed -> Signed
signum Zero = Zero
signum (NonZero s _) = NonZero s Positive.one

fromInteger :: Integer -> Signed
fromInteger x = case compare x 0 of
    EQ -> Zero
    LT -> Minus $ Positive.fromInteger $ Num.abs x
    GT -> Plus  $ Positive.fromInteger x

toInteger :: Signed -> Integer
toInteger Zero = 0
toInteger (Plus x) = Positive.toInteger x
toInteger (Minus x) = Num.negate $ Positive.toInteger x

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
