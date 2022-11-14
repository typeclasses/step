{-# language Safe #-}

module Integer.Conversion
  (
    IntegerNarrow (narrow),
    IntegerConvert (convert),
    IntegerEquiv,
  )
  where

import Data.Function ((.), id)
import Data.Maybe (Maybe (..))
import Integer.Integer (Integer)
import Integer.Natural (Natural)
import Integer.Positive (Positive)
import Integer.Signed (Signed)

import qualified Integer.Integer as Integer
import qualified Integer.Natural as Natural
import qualified Integer.Positive as Positive
import qualified Integer.Signed as Signed

class IntegerNarrow a b => IntegerConvert a b where
    convert :: a -> b

class IntegerNarrow a b where
    narrow :: a -> Maybe b

class (IntegerConvert a b, IntegerConvert b a) => IntegerEquiv a b


---  Isomorphisms  ---

instance IntegerEquiv   Integer  Integer
instance IntegerConvert Integer  Integer  where convert = id
instance IntegerNarrow  Integer  Integer  where narrow = Just

instance IntegerEquiv   Natural  Natural
instance IntegerConvert Natural  Natural  where convert = id
instance IntegerNarrow  Natural  Natural  where narrow  = Just

instance IntegerEquiv   Positive Positive
instance IntegerConvert Positive Positive where convert = id
instance IntegerNarrow  Positive Positive where narrow  = Just

instance IntegerEquiv   Signed   Signed
instance IntegerConvert Signed   Signed   where convert = id
instance IntegerNarrow  Signed   Signed   where narrow  = Just

instance IntegerEquiv   Integer  Signed
instance IntegerConvert Integer  Signed   where convert = Integer.toSigned
instance IntegerNarrow  Integer  Signed   where narrow  = Just . convert

instance IntegerEquiv   Signed   Integer
instance IntegerConvert Signed   Integer  where convert = Signed.toInteger
instance IntegerNarrow  Signed   Integer  where narrow  = Just . convert


---  Prisms  ---

instance IntegerNarrow  Integer  Natural  where narrow  = Integer.toNatural
instance IntegerNarrow  Natural  Integer  where narrow  = Just . convert
instance IntegerConvert Natural  Integer  where convert = Natural.toInteger

instance IntegerNarrow  Signed   Natural  where narrow  = Signed.toNatural
instance IntegerNarrow  Natural  Signed   where narrow  = Just . convert
instance IntegerConvert Natural  Signed   where convert = Natural.toSigned

instance IntegerNarrow  Integer  Positive where narrow  = Integer.toPositive
instance IntegerNarrow  Positive Integer  where narrow  = Just . convert
instance IntegerConvert Positive Integer  where convert = Positive.toInteger

instance IntegerNarrow  Natural  Positive where narrow  = Natural.toPositive
instance IntegerNarrow  Positive Natural  where narrow  = Just . convert
instance IntegerConvert Positive Natural  where convert = Positive.toNatural

instance IntegerNarrow  Signed   Positive where narrow  = Signed.toPositive
instance IntegerNarrow  Positive Signed   where narrow  = Just . convert
instance IntegerConvert Positive Signed   where convert = Positive.toSigned
