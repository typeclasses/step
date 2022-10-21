module Step.Walk where

import Step.Interface

import Data.Functor (Functor (..))
import Data.Function ((.))
import Control.Monad (Monad (..))
import Control.Applicative (Applicative (..))
import SupplyChain (Factory, order)
import SupplyChain.Interface.Resettable (IsResettable (reset))

import qualified Control.Monad as Monad

{- |
    A Walk is a factory with 'Step' as its upstream interface, with the
    additional implication that a walk is implicitly preceded and followed
    by a 'StepReset'. Sequencing operations like '(<*>)' and '(>>=)' insert
    resets between the operations. (The implicit resets and the idempotency
    of 'StepReset' are essential to arguing that the 'Applicative' and
    'Monad' class laws are respected.)
-}

-- todo: rename, update doc

newtype Walk up action a =
    Walk (Factory up action a)
    deriving newtype Functor

instance IsResettable up => Applicative (Walk up action)
  where
    pure = Walk . pure
    (<*>) = Monad.ap

instance IsResettable up => Monad (Walk up action)
  where
    Walk step1 >>= step2 = Walk do
        x <- step1
        order reset
        case step2 x of Walk b' -> b'
