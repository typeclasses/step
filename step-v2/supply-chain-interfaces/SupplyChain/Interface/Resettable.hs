module SupplyChain.Interface.Resettable where

import Essentials
import SupplyChain

import qualified Control.Monad as Monad

class IsResettable i where
    -- | Idempotent
    reset :: i ()

{-| A job with a resettable upstream interface, with the additional implication
    that a resetting sequence is implicitly preceded and followed by a 'reset'

Sequencing operations like '(<*>)' and '(>>=)' insert resets between the operations.
(The implicit resets and the idempotency of 'reset' are essential to arguing that
the 'Applicative' and 'Monad' class laws are sufficiently respected.) -}
newtype ResettingSequence up action a =
    ResettingSequenceJob{ resettingSequenceJob :: Job up action a }
    deriving newtype Functor

instance IsResettable up => Applicative (ResettingSequence up action) where
    pure = ResettingSequenceJob . pure
    (<*>) = Monad.ap

instance IsResettable up => Monad (ResettingSequence up action) where
    step1 >>= step2 = ResettingSequenceJob do
        x <- resettingSequenceJob step1
        order reset
        resettingSequenceJob (step2 x)
