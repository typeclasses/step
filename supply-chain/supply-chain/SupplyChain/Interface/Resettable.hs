module SupplyChain.Interface.Resettable where

import SupplyChain

import Data.Functor (Functor (..))
import Data.Function ((.))
import Control.Monad (Monad (..))
import Control.Applicative (Applicative (..))

import qualified Control.Monad as Monad

class IsResettable (i :: Interface)
  where
    -- | Idempotent
    reset :: i ()

{- |
    A /resetting sequence/ is a factory with a resettable upstream interface,
    with the additional implication that a resetting sequence is implicitly
    preceded and followed by a 'reset'. Sequencing operations like '(<*>)' and
    '(>>=)' insert resets between the operations. (The implicit resets and the
    idempotency of 'reset' are essential to arguing that the 'Applicative'
    and 'Monad' class laws are sufficiently respected.)
-}

newtype ResettingSequence up action a =
    ResettingSequence (Factory up action a)
    deriving newtype Functor

instance IsResettable up => Applicative (ResettingSequence up action)
  where
    pure = ResettingSequence . pure
    (<*>) = Monad.ap

instance IsResettable up => Monad (ResettingSequence up action)
  where
    ResettingSequence step1 >>= step2 = ResettingSequence do
        x <- step1
        order reset
        case step2 x of ResettingSequence b' -> b'
