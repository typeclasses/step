module Step.Walk where

import Step.Interface.Core

import Data.Functor (Functor (..))
import Data.Function ((.))
import Control.Monad (Monad (..))
import Control.Applicative (Applicative (..))
import SupplyChain (Factory, order)

import qualified Control.Monad as Monad

{- |
    A Walk is a factory with 'Step' as its upstream interface, with the
    additional implication that a walk is implicitly preceded and followed
    by a 'StepReset'. Sequencing operations like '(<*>)' and '(>>=)' insert
    resets between the operations. (The implicit resets and the idempotency
    of 'StepReset' are essential to arguing that the 'Applicative' and
    'Monad' class laws are respected.)
-}

newtype Walk mode chunk action a =
    Walk (Factory (Step mode chunk) action a)
    deriving newtype Functor

instance Applicative (Walk mode chunk action)
  where
    pure = Walk . pure
    (<*>) = Monad.ap

instance Monad (Walk mode chunk action)
  where
    Walk step1 >>= step2 = Walk do
        x <- step1
        order StepReset
        case step2 x of Walk b' -> b'
