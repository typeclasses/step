module Integer.Subtraction
  (
    Subtraction (subtractInteger, subtractSigned),
    Subtraction' (subtract),
  )
  where

import Integer.Integer (Integer)
import Integer.Natural (Natural)
import Integer.Positive (Positive)
import Integer.Signed (Signed)

import qualified Integer.Natural as Natural
import qualified Integer.Positive as Positive
import qualified Integer.Signed as Signed
import qualified Prelude as Num (Num (..))

-- | Domain of a subtraction operation
class Subtraction a where
    subtractInteger :: a -> a -> Integer
    subtractInteger a b = Signed.toInteger (subtractSigned a b)

    subtractSigned  :: a -> a -> Signed
    subtractSigned a b = Signed.fromInteger (subtractInteger a b)

instance Subtraction Integer where
    subtractInteger = (Num.-)

instance Subtraction Signed where
    subtractInteger a b = (Num.-) (Signed.toInteger a) (Signed.toInteger b)
    subtractSigned = (Num.-)

instance Subtraction Natural where
    subtractSigned = Natural.subtract

instance Subtraction Positive where
    subtractSigned = Positive.subtract

-- | Codomain of a subtraction operation
class Subtraction' b where
    subtract :: Subtraction a => a -> a -> b

instance Subtraction' Integer where
    subtract = subtractInteger

instance Subtraction' Signed where
    subtract = subtractSigned
