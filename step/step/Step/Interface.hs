module Step.Interface
  (
    -- * The interface
    Mode (..), Step (..), AdvanceResult (..), stepCast,

    -- * Factories
    commit, reset, peekSomeMaybe, peekCharMaybe, atEnd,

  )
  where

import Step.Chunk
import Step.Interface.Core

-- The basics
import Data.Bool (Bool (..))
import Data.Maybe (Maybe (..), isNothing)
import Data.Functor (Functor (..), (<&>))

-- Math
import Numeric.Natural (Natural)
import NatOptics.Positive (Positive)

-- Streaming
import SupplyChain (Factory)
import qualified SupplyChain

reset :: forall c m mode. Factory (Step mode c) m ()
reset = SupplyChain.order StepReset

commit :: forall c m. Positive Natural -> Factory (Step 'RW c) m AdvanceResult
commit n = SupplyChain.order (StepCommit n)

peekSomeMaybe :: forall c m mode. Factory (Step mode c) m (Maybe c)
peekSomeMaybe = SupplyChain.order StepNext

peekCharMaybe :: forall c m mode. Chunk c => Factory (Step mode c) m (Maybe (One c))
peekCharMaybe = peekSomeMaybe <&> fmap @Maybe head

atEnd :: forall c m mode. Factory (Step mode c) m Bool
atEnd = peekSomeMaybe <&> isNothing
