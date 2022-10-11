module Step.Walk.Core where

import Step.Interface.Core

-- The basics
import Data.Maybe (Maybe (..))
import Data.Functor (Functor (..), (<$>))
import Data.Function (($), (.), id)
import Data.Either (Either (..))
import Control.Monad (Monad (..))
import qualified Control.Monad as Monad
import Control.Applicative (Applicative (..))
import Data.Kind (Type)
import Prelude (error)

-- Math
import Numeric.Natural (Natural)
import NatOptics.Positive.Unsafe (Positive)

-- Transformers
import qualified Control.Monad.Reader as MTL
import Control.Monad.Trans.Except (ExceptT (..))
import qualified Control.Monad.Trans.Except as MTL

-- Streaming
import SupplyChain (Factory, (>->), order)
import qualified SupplyChain

-- Etc
import GHC.TypeLits (TypeError, ErrorMessage (Text))


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
